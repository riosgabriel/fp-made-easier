module Ch5
  ( ($)
  , apply
  , applyFlipped
  , const
  , flip
  , test
  )
  where

import Data.Eq

import Data.List (List(..), (:))
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, show, discard)

flip :: forall a b c. (a -> b -> c) -> b -> a -> c
flip f x y = f y x
-- an alternative version using lambda function
-- flip f = \x y -> f y x

const :: forall a b. a -> b -> a
const x _ = x
-- an alternative version using lambda function
-- const = \x y -> x

apply :: forall a b. (a -> b) -> a -> b
apply f x = f x
-- an alternative version just returning the function
-- apply f = f

infixr 0 apply as $

applyFlipped :: forall a b. a -> (a -> b) -> b
applyFlipped x f = f x
-- an alternative
-- applyFlipped = flip apply

infixl 1 applyFlipped as #

singleton :: forall a. a -> List a
singleton x = x : Nil
-- an alternative using prefixed version
-- singleton x = Cons x Nil

null :: forall a. List a -> Boolean
null Nil = true
null _ = false

snoc :: forall a. List a -> a -> List a
snoc Nil y = singleton y
snoc (x:xs) y = x : snoc xs y


test :: Effect Unit
test = do
  log $ show $ flip const 1 2
  log $ flip const 1 2 # show
  flip const 1 2 # show # log
  log $ show $ const 1 2 == 1
  log $ show $ singleton "xyz"
  log $ show $ null Nil
  log $ show $ null ("abc" : Nil)
  log $ show $ snoc (1 : 2 : Nil) 3
  log $ show $ snoc Nil 9
