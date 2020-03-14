# incremental: incremental update interface

[![Hackage](https://img.shields.io/hackage/v/incremental.svg)](https://hackage.haskell.org/package/incremental)
![Haskell CI](https://github.com/fumieval/incremental/workflows/Haskell%20CI/badge.svg)
[![Discord](https://img.shields.io/discord/664807830116892674?color=%237095ec&label=Discord&style=plastic)](https://discord.gg/DG93Tgs)

This package provides a typeclass for incremental updates and diffing.

```haskell
class Incremental a where
  -- | the difference type
  type Delta a
  -- | @'maybe' a ('patch' a) ('diff' b a) ≡ b@
  patch :: a -> Delta a -> a
  -- | returns 'Nothing' when there is no update
  diff :: a -> a -> Maybe (Delta a)
```