{-| I intend to use these operators in Miu instead of the usual Haskell ones
   so I thought, maybe I should "test drive" them for a while before putting
   them in.
-}
module Sanna.Prelude
  ( module Data.Maybe
  , (|>), (>|>), (<|), (<|<), (.>), (<.)
  ) where

import Data.Maybe

infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

infixl 4 >|>
(>|>) :: Functor f => f a -> (a -> b) -> f b
(>|>) = flip (<|<)

infixr 0 <|
(<|) :: (a -> b) -> a -> b
f <| x = f x

infixr 4 <|<
(<|<) :: Functor f => (a -> b) -> f a -> f b
(<|<) = (<$>)

infixl 9 .>
(.>) :: (a -> b) -> (b -> c) -> a -> c
f .> g = g . f

infixr 9 <.
(<.) :: (b -> c) -> (a -> b) -> a -> c
f <. g = f . g
