{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.Incremental (
  Incremental(..)
  , Alter(..)
  , Hetero(..)
  , Fresh(..)
) where

import Control.DeepSeq
import Data.Semigroup hiding (diff)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void
import Data.Int
import Data.Word
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

data Alter a = Insert a | Update (Delta a) | Delete | Upsert a (Delta a)
  deriving Generic

deriving instance (Show a, Show (Delta a)) => Show (Alter a)
instance (NFData a, NFData (Delta a)) => NFData (Alter a)

instance (Incremental a, Semigroup (Delta a)) => Semigroup (Alter a) where
  _ <> Insert a = Insert a
  _ <> Delete = Delete
  Insert a <> Update d = Insert (patch a d)
  Insert a <> Upsert _ d = Insert (patch a d)
  Update c <> Update d = Update (c <> d)
  Update c <> Upsert a d = Upsert a (c <> d)
  Delete <> Update _ = Delete
  Delete <> Upsert a _ = Insert a
  Upsert a d <> Update e = Upsert a (d <> e)
  Upsert a d <> Upsert _ e = Upsert a (d <> e)

instance (Incremental a, Semigroup (Delta a), Monoid (Delta a)) => Monoid (Alter a) where
  mempty = Update mempty
  mappend = (<>)

instance Incremental a => Incremental (Maybe a) where
  type Delta (Maybe a) = Alter a
  patch _ (Insert a) = Just a
  patch (Just a) (Update d) = Just $! patch a d
  patch (Just a) (Upsert _ d) = Just $! patch a d
  patch _ (Upsert a _) = Just a
  patch _ _ = Nothing
  diff Nothing (Just a) = Just $ Insert a
  diff (Just a) (Just b) = Update <$> diff a b
  diff (Just _) Nothing = Just Delete
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
  type Delta (IntMap.IntMap a) = IntMap.IntMap (Alter a)
  patch = IntMap.mergeWithKey (\_ a -> patch (Just a)) id
    $ IntMap.mapMaybe $ \case
      Insert a -> Just a
      Upsert a _ -> Just a
      _ -> Nothing
  diff = (check.). IntMap.mergeWithKey (\_ a b -> Update <$> diff a b) (IntMap.map (const Delete)) (IntMap.map Insert)
    where
      check m = if IntMap.null m then Nothing else Just m

instance (Ord k, Incremental a) => Incremental (Map.Map k a) where
  type Delta (Map.Map k a) = Map.Map k (Alter a)
  patch = Map.mergeWithKey (\_ a -> patch (Just a)) id
    $ Map.mapMaybe $ \case
      Insert a -> Just a
      Upsert a _ -> Just a
      _ -> Nothing
  diff = (check.). Map.mergeWithKey (\_ a b -> Update <$> diff a b) (Map.map (const Delete)) (Map.map Insert)
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

newtype Hetero a = Hetero { getHetero :: a }
  deriving (Bounded, Enum, Eq, Floating, Fractional, Integral, Semigroup
      , Monoid, Num, Ord, Real, RealFrac, RealFloat, Generic, NFData)

-- | 'diff' checks equality
instance Eq a => Incremental (Hetero a) where
  type Delta (Hetero a) = a
  patch _ = Hetero
  diff (Hetero a) (Hetero b)
    | a /= b = Just b
    | otherwise = Nothing

newtype Fresh a = Fresh { getFresh :: a }
  deriving (Bounded, Enum, Eq, Floating, Fractional, Integral, Semigroup
      , Monoid, Num, Ord, Real, RealFrac, RealFloat, Generic, NFData)

-- | Always updated
instance Incremental (Fresh a) where
  type Delta (Fresh a) = a
  patch _ = Fresh
  diff _ = Just . getFresh

#define TRIVIAL_EQ(ty) instance Incremental ty where { \
  type Delta ty = ty; \
  patch _ x = x; \
  diff a b = if a /= b then Just b else Nothing; \
  }

TRIVIAL_EQ(Bool)
TRIVIAL_EQ(Char)
TRIVIAL_EQ(Double)
TRIVIAL_EQ(Float)
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
