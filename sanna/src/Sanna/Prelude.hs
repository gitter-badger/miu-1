{-| I intend to use these operators in Miu instead of the usual Haskell ones
   so I thought, maybe I should "test drive" them for a while before putting
   them in.
-}
module Sanna.Prelude
  ( module Control.Applicative
  , module Control.Exception
  , module Control.Monad
  , module Data.Coerce
  , module Data.Monoid
  , module Data.Maybe
  , module Data.String
  , (|>), (>|>), (<|), (<|<), (.>), (<.), (<.:), unreachable
  ) where

import Control.Applicative
import Control.Exception (assert)
import Control.Monad
import Data.String (IsString (..))
import Data.Coerce
import Data.Monoid
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

infixr 8 <.:
(<.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(<.:) = (.) . (.)

unreachable :: a
unreachable = error "Unreachable!"
