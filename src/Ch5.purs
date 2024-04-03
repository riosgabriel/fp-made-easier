module Ch5
  ( ($)
  , apply
  , applyFlipped
  , const
  , flip
  , test
  )
  where

import Data.Eq ((==))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Console (log)
import Prelude (type (~>), Unit, discard, max, negate, show, (+), (-), (<), (<<<), (>>>), (<=), (>), (>=))
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

-- null checks if a list is empty
null :: forall a. List a -> Boolean
null Nil = true
null _ = false

-- snoc is the reverse of cons
-- puts an element at the end of a List
snoc :: forall a. List a -> a -> List a
snoc Nil y = singleton y
snoc (x:xs) y = x : snoc xs y

length :: forall a. List a -> Int
-- length Nil = 0
-- length (_:xs) = 1 + length xs
length l = inner 0 l where
  inner :: Int -> List a -> Int
  inner acc Nil = acc
  inner acc (_:xs) = inner (acc + 1) xs


tailLength :: forall a. List a -> Int -> Int
tailLength Nil acc = acc
tailLength (_:xs) acc = tailLength xs (acc + 1)

head :: forall a. List a -> Maybe a
head Nil = Nothing
head (x:_) = Just x

tail :: forall a. List a -> Maybe (List a)
tail (_:xs) = Just xs
tail _ = Nothing

last :: forall a. List a -> Maybe a
last Nil = Nothing
last (x:Nil) = Just x
last (_:xs) = last xs

init :: forall a. List a -> Maybe (List a)
init Nil = Nothing
init (_:Nil) = Just Nil
init (x:xs) = Just $ x : case init xs of
  Just n -> n
  Nothing -> Nil

-- another version of init with an inner function
-- init :: forall a. List a -> Maybe (List a)
-- init Nil = Nothing
-- init l = Just $ go l where
--   go Nil = Nil
--   go (_ : Nil) = Nil
--   go (x:xs) = x : go xs

-- it is the opposite of cons. instead of taking head and tail and producing a List, uncons
-- takes a list and return a Record containg head and tail
uncons :: forall a. List a -> Maybe { head :: a, tail :: List a }
uncons (x:xs) = Just $ { head: x, tail: xs }
uncons _ = Nothing

index :: forall a. List a -> Int -> Maybe a
index Nil _ = Nothing
index _ i | i < 0 = Nothing
index (x:_) 0 = Just x
index (_:xs) i = index xs (i - 1)

infixl 8 index as !!

findIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex pred list = go list 0 where
  go Nil _ = Nothing
  go (x:xs) n = if pred x then Just n else go xs (n + 1)

findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex pred list = go Nothing 0 list  where
  go lastFound _ Nil = lastFound
  go _ n (x:xs) = go (if pred x then Just n else Nothing) (n + 1) xs

reverse :: List ~> List
reverse l = go l Nil where 
  go Nil nl = nl
  go (x:xs) nl = go xs $ x : nl

concat :: forall a. List (List a) -> List a
concat Nil = Nil
concat (Nil:xss) = concat xss
concat ((x:xs) : xss) = x : concat (xs:xss)

filter :: forall a. (a -> Boolean) -> List a -> List a
filter _ Nil = Nil
filter f (x:xs) = if f x then x : filter f xs else filter f xs

filterTail :: forall a. (a -> Boolean) -> List a -> List a
-- non-Point-free version
-- filterTail pred l = reverse $ go Nil l where
filterTail pred = reverse <<< go Nil where
  go acc Nil = acc
  go acc (x:xs) = go (if pred x then x : acc else acc) xs

catMaybes :: forall a. List (Maybe a) -> List a
catMaybes Nil = Nil
catMaybes (x:xs) = case x of
  Just y -> y : catMaybes xs
  Nothing -> catMaybes xs
-- tailRecursion version
-- catMaybes lom = reverse $ go Nil lom where
--   go acc Nil = acc
--   go acc (x:xs) = case x of
--     Just y -> go (y:acc) xs
--     Nothing -> go acc xs

range :: Int -> Int -> List Int
-- range x y | x == y = singleton y
-- range x y | y < x = x: range (x - 1) y
-- range x y = x : range (x + 1) y
range x y = go x y where
  go x' y' | x' == y' = singleton y'
  go x' y' = x' : range (x' + step) y'
  step = if y > x then 1 else (-1)

take :: forall a. Int -> List a -> List a
-- take n _ | n < 1 = Nil
-- take _ Nil = Nil
-- take n (x:xs) = x : take (n - 1) xs
take n = reverse <<< go Nil (max n 0) where
  go acc _ Nil = acc
  go acc 0 _ = acc
  go acc n' (x:xs) = go (x:acc) (n' - 1) xs

drop :: forall a. Int -> List a -> List a
drop n l | n < 0 = l
drop _ Nil = Nil
drop 0 l = l
drop n (_:xs) = drop (n - 1) xs

takeWhile :: forall a. (a -> Boolean) -> List a -> List a
takeWhile _ Nil = Nil
takeWhile pred (x:xs) = if pred x then x : takeWhile pred xs else Nil

dropWhile :: forall a. (a -> Boolean) -> List a -> List a
dropWhile _ Nil = Nil
dropWhile pred l@(x:xs) = if pred x then dropWhile pred xs else l

takeEnd :: forall a. Int -> List a -> List a
takeEnd n = go >>> snd where
  go Nil = Tuple 0 Nil
  go (x:xs) = go xs # \(Tuple c nl) -> Tuple (c+1) $ if c < n then x : nl else nl
-- simpler alternative but traversing the list 3 times
-- takeEnd n _ | n < 1 = Nil
-- takeEnd n l = reverse $ take n $ reverse l


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

  log $ show $ length $ 1:2:3:Nil
  log $ show $ tailLength (1:2:3:Nil) 0

  log $ show $ head (1:2:3:Nil)
  log $ show $ head (Nil :: List Unit)

  log $ show $ tail (1:2:3:Nil)
  log $ show $ tail (Nil :: List Unit)
  
  log $ show $ last (1:2:3:Nil)
  log $ show $ last (Nil :: List Unit)

  log $ show $ init (1:2:3:Nil)
  log $ show $ init (Nil :: List Unit)

  log $ show $ uncons (1:2:3:Nil)
  log $ show $ uncons (Nil :: List Unit)

  log $ show $ index (1:2:3:Nil) 2 == Just 3
  log $ show $ index (1:2:3:Nil) 1 == Just 2
  log $ show $ index (1:2:3:Nil) 0 == Just 1
  log $ show $ index (Nil :: List Unit) 2 == Nothing
  log $ show $ index (1:2:3:Nil) (-1) == Nothing

  log $ show $ (1:2:3:Nil) !! 1 == Just 2
  log $ show $ (1:2:3:Nil) !! 0 == Just 1

  log $ show $ findIndex (_ > 2) (1:2:3:Nil) == Just 2
  log $ show $ findIndex (_ > 99) (1:2:3:Nil) == Nothing
  log $ show $ findIndex (_ >= 2) (Nil) == Nothing

  log $ show $ findLastIndex (_ == 10) (Nil :: List Int) == Nothing
  log $ show $ findLastIndex (_ == 10) (10:5:10:(-1):2:10:Nil) == Just 5
  log $ show $ findLastIndex (_ == 10) (11:12:Nil) == Nothing

  log $ show $ reverse (10:20:30:Nil) == (30:20:10:Nil)

  log $ show $ filter (4 > _) (1:2:3:4:5:6:7:8:Nil) == (1:2:3:Nil)

  log $ show $ filterTail (4 > _) (1:2:3:4:5:6:7:8:Nil) == (1:2:3:Nil)

  log $ show $ catMaybes (Just 1:Nothing:Just 2:Nothing:Nothing:Just 5:Nil) == (1:2:5:Nil)

  log $ show $ range 1 5 == (1:2:3:4:5:Nil)
  log $ show $ range 1 (-1) == (1:0:(-1):Nil)
  log $ show $ range 0 0 == (0:Nil)

  log $ show $ take 3 (1:2:3:4:5:Nil) == (1:2:3:Nil)
  log $ show $ take 10 (1:2:3:4:Nil) == (1:2:3:4:Nil)
  log $ show $ take 0 (1:2:3:4:Nil) == Nil
  log $ show $ take 5 (Nil :: List Int) == Nil
  log $ show $ take (-5) (1:2:3:4:Nil) == Nil

  log $ show $ drop 99 (1:2:3:4:5:Nil) == Nil
  log $ show $ drop 2 (Nil :: List Int) == Nil
  log $ show $ drop 2 (1:2:3:4:5:Nil) == (3:4:5:Nil)
  log $ show $ drop 0 (1:2:3:4:5:Nil) == (1:2:3:4:5:Nil)
  log $ show $ drop (-1) (1:2:3:4:5:Nil) == (1:2:3:4:5:Nil)

  log $ show $ takeWhile (_ <= 2) (1:2:3:Nil) == (1:2:Nil)

  log $ show $ dropWhile (_ <= 2) (1:2:3:Nil) == (3:Nil)
  log $ show $ dropWhile (_ <= 1) (3:10:2:Nil) == (3:10:2:Nil)

  log $ show $ takeEnd 3 (1:2:3:4:5:6:Nil) == (4:5:6:Nil)
  log $ show $ takeEnd 2 (1:2:3:4:5:6:Nil) == (5:6:Nil)
  log $ show $ takeEnd 10 (1:2:3:Nil) == (1:2:3:Nil)
  log $ show $ takeEnd 0 (1:2:3:Nil) == Nil
  log $ show $ takeEnd 1 (1:2:3:Nil) == (3:Nil)
  log $ show $ takeEnd (-1) (1:2:3:Nil) == Nil
