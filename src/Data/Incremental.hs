{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PolyKinds #-}
module Data.Incremental (
  Incremental(..)
  , Alter(..)
  , Hetero(..)
  , Fresh(..)
  , WrapDelta(..)
) where

import Control.Applicative
import Control.DeepSeq
import qualified Data.Aeson as J
import Data.ByteString (ByteString)
import Data.Fixed
import Data.Functor.Identity
import Data.Semigroup hiding (diff)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Proxy
import qualified Data.Set as Set
import Data.Void
import Data.Int
import Data.Word
import Data.Text (Text)
import Numeric.Natural
import GHC.Generics

class Incremental a where
  -- | the difference type
  type Delta a
  -- | @'maybe' a ('patch' a) ('diff' b a) ≡ b@
  patch :: a -> Delta a -> a
  -- | returns 'Nothing' when there is no update
  diff :: a -> a -> Maybe (Delta a)

instance Incremental () where
  type Delta () = Void
  patch _ _ = ()
  diff _ _ = Nothing

instance Incremental Void where
  type Delta Void = Void
  patch v _ = v
  diff _ _ = Nothing

instance Incremental (Proxy a) where
  type Delta (Proxy a) = Void
  patch v _ = v
  diff _ _ = Nothing

instance Incremental a => Incremental (Identity a) where
  type Delta (Identity a) = Delta a
  patch (Identity a) d = Identity (patch a d)
  diff (Identity a) (Identity b) = diff a b

instance Incremental a => Incremental (Const a b) where
  type Delta (Const a b) = Delta a
  patch (Const a) d = Const (patch a d)
  diff (Const a) (Const b) = diff a b

data Alter a d = Insert a
  | Update d
  | Delete a -- ^ last value
  | Delete_
  | Upsert a d
  deriving (Generic, Functor)

deriving instance (Show a, Show d) => Show (Alter a d)
instance (NFData a, NFData d) => NFData (Alter a d)

instance (J.FromJSON a, J.FromJSON d) => J.FromJSON (Alter a d)
instance (J.ToJSON a, J.ToJSON d) => J.ToJSON (Alter a d)

instance (Incremental a, d ~ Delta a, Semigroup d) => Semigroup (Alter a d) where
  _ <> Insert a = Insert a
  _ <> Delete a = Delete a
  _ <> Delete_ = Delete_
  Insert a <> Update d = Insert (patch a d)
  Insert a <> Upsert _ d = Insert (patch a d)
  Update c <> Update d = Update (c <> d)
  Update c <> Upsert a d = Upsert a (c <> d)
  Delete a <> Update _ = Delete a
  Delete _ <> Upsert a _ = Insert a
  Delete_ <> Update _ = Delete_
  Delete_ <> Upsert a _ = Insert a
  Upsert a d <> Update e = Upsert (patch a e) (d <> e)
  Upsert a d <> Upsert _ e = Upsert a (d <> e)

instance (Incremental a, Monoid d, d ~ Delta a) => Monoid (Alter a d) where
  mempty = Update mempty
  mappend = (<>)

instance Incremental a => Incremental (Maybe a) where
  type Delta (Maybe a) = Alter a (Delta a)
  patch _ (Insert a) = Just a
  patch (Just a) (Update d) = Just $! patch a d
  patch (Just a) (Upsert _ d) = Just $! patch a d
  patch _ (Upsert a _) = Just a
  patch _ _ = Nothing
  diff Nothing (Just a) = Just $ Insert a
  diff (Just a) (Just b) = Update <$> diff a b
  diff (Just a) Nothing = Just (Delete a)
  diff _ _ = Nothing

instance (Incremental a, Incremental b) => Incremental (a, b) where
  type Delta (a, b) = (Maybe (Delta a), Maybe (Delta b))
  patch (a, b) (d, e) = (maybe a (patch a) d, maybe b (patch b) e)
  diff (a, b) (c, d) = Just (diff a c, diff b d)

instance (Incremental a, Incremental b, Incremental c) => Incremental (a, b, c) where
  type Delta (a, b, c) = (Maybe (Delta a), Maybe (Delta b), Maybe (Delta c))
  patch (a, b, c) (d, e, f) = (maybe a (patch a) d, maybe b (patch b) e, maybe c (patch c) f)
  diff (a, b, c) (d, e, f) = Just (diff a d, diff b e, diff c f)

instance (Incremental a) => Incremental (IntMap.IntMap a) where
  type Delta (IntMap.IntMap a) = IntMap.IntMap (Alter a (Delta a))
  patch = IntMap.mergeWithKey (\_ a -> patch (Just a)) id
    $ IntMap.mapMaybe $ \case
      Insert a -> Just a
      Upsert a _ -> Just a
      _ -> Nothing
  diff = (check.). IntMap.mergeWithKey (\_ a b -> Update <$> diff a b) (IntMap.map Delete) (IntMap.map Insert)
    where
      check m = if IntMap.null m then Nothing else Just m

instance (Ord k, Incremental a) => Incremental (Map.Map k a) where
  type Delta (Map.Map k a) = Map.Map k (Alter a (Delta a))
  patch = Map.mergeWithKey (\_ a -> patch (Just a)) id
    $ Map.mapMaybe $ \case
      Insert a -> Just a
      Upsert a _ -> Just a
      _ -> Nothing
  diff = (check.). Map.mergeWithKey (\_ a b -> Update <$> diff a b) (Map.map Delete) (Map.map Insert)
    where
      check m = if Map.null m then Nothing else Just m

instance Ord a => Incremental (Set.Set a) where
  -- | (insertion, deletion)
  type Delta (Set.Set a) = (Set.Set a, Set.Set a)
  patch s (ins, del) = s `Set.difference` del `Set.union` ins
  diff a b = check $ go (Set.toAscList a) (Set.toAscList b) Set.empty Set.empty where
    go xs'@(x:xs) ys'@(y:ys) s t = case compare x y of
      LT -> go xs ys' s (Set.insert x t)
      EQ -> go xs ys s t
      GT -> go xs' ys (Set.insert y s) t
    go xs ys s t = (foldr Set.insert s ys, foldr Set.insert t xs)
    check r@(s, t)
      | Set.null s, Set.null t = Nothing
      | otherwise = Just r

instance Num a => Incremental (Sum a) where
  type Delta (Sum a) = Sum a
  patch = (<>)
  diff (Sum a) (Sum b) = Just $ Sum (b - a)

instance Eq a => Incremental [a] where
  type Delta [a] = [a]
  patch _ xs = xs
  diff a b
    | a == b = Nothing
    | otherwise = Just b

newtype Hetero a = Hetero { getHetero :: a }
  deriving (Bounded, Enum, Eq, Floating, Fractional, Integral, Semigroup
      , Monoid, Num, Ord, Real, RealFrac, RealFloat, Generic, NFData, J.FromJSON, J.ToJSON)

-- | 'diff' checks equality
instance Eq a => Incremental (Hetero a) where
  type Delta (Hetero a) = a
  patch _ = Hetero
  diff (Hetero a) (Hetero b)
    | a /= b = Just b
    | otherwise = Nothing

newtype Fresh a = Fresh { getFresh :: a }
  deriving (Bounded, Enum, Eq, Floating, Fractional, Integral, Semigroup
      , Monoid, Num, Ord, Real, RealFrac, RealFloat, Generic, NFData, J.FromJSON, J.ToJSON)

-- | Always updated
instance Incremental (Fresh a) where
  type Delta (Fresh a) = a
  patch _ = Fresh
  diff _ = Just . getFresh

#define TRIVIAL_EQ(ty) instance Incremental (ty) where { \
  type Delta (ty) = ty; \
  patch _ x = x; \
  diff a b = if a /= b then Just b else Nothing; \
  }

-- | Wrap the result of 'diff'. Useful in combination with HKD
newtype WrapDelta h x = WrapDelta {unwrapDelta :: Maybe (Delta (h x))}

deriving instance Show (Delta (h x)) => Show (WrapDelta h x)
deriving instance Eq (Delta (h x)) => Eq (WrapDelta h x)
deriving instance Ord (Delta (h x)) => Ord (WrapDelta h x)
deriving instance J.FromJSON (Delta (h x)) => J.FromJSON (WrapDelta h x)
deriving instance J.ToJSON (Delta (h x)) => J.ToJSON (WrapDelta h x)

TRIVIAL_EQ(Bool)
TRIVIAL_EQ(Char)
TRIVIAL_EQ(Double)
TRIVIAL_EQ(Float)
TRIVIAL_EQ(Fixed a)
TRIVIAL_EQ(Int)
TRIVIAL_EQ(Int8)
TRIVIAL_EQ(Int16)
TRIVIAL_EQ(Int32)
TRIVIAL_EQ(Int64)
TRIVIAL_EQ(Integer)
TRIVIAL_EQ(Natural)
TRIVIAL_EQ(Ordering)
TRIVIAL_EQ(Word)
TRIVIAL_EQ(Word8)
TRIVIAL_EQ(Word16)
TRIVIAL_EQ(Word32)
TRIVIAL_EQ(Word64)
TRIVIAL_EQ(Text)
TRIVIAL_EQ(ByteString)
