{-# LANGUAGE GADTSyntax #-}
module Main where

-- import           Lib

main :: IO ()
main = print "hi" -- someFunc



{-
multi line comment starts the demo code
-}

-- notice many of these don't have type signatures.
-- Haskell is smart enough figure out 99% of the type signatures for you.
-- any type signatures here are just to add clarity.

-- number
somenumber = 1

-- string
somestring = "somestring"

-- type signature
add1 :: Int -> Int
-- function definition for that type sig
add1 x = x + 1


-- anonymous function (we give this one a name anyway)
add2 = (\x -> x + 2)

-- multi-arg function
add :: Int -> (Int -> Int)
add x y = x + y

-- since -> in a type signature is right associative
-- the type signature above really means add :: Int -> ( Int -> Int )
-- since the type of a function from a to b is (a -> b)
-- that means you can give one argument, and get back a new function
-- that's called partial application of a function

-- partial application
addOne :: Int -> Int
addOne = add 1

-- define your own datatype

-- data Temp = Cold | Hot
data Temp where
  Cold :: Temp
  Hot :: Temp
-- data Season = Winter | Spring | Summer | Fall deriving (Show,Enum)
data Season where
  Winter :: Season
  Spring :: Season
  Summer :: Season
  Fall :: Season
    deriving (Show, Enum)
-- 'Show' is how you turn a value into a string for display
-- deriving means "write the show code for me"

mySeason :: Season
mySeason = Summer

-- type aliases
-- type Name = String
-- type Age = Int

-- more complex types
-- data People = Person String Int
data People where
  Person :: String -> Int -> People

-- polymorphic (parameterized) datatype
-- data Tree a = Nil | Node (Tree a) a (Tree a) deriving Show
data Tree a where
  Nil :: Tree a
  Node :: Tree a -> a -> Tree a -> Tree a

-- data IntTree = NilI | NodeI (IntTree) Int (IntTree) deriving Show
data IntTree where
  NilI :: IntTree
  NodeI :: IntTree -> Int -> IntTree
    deriving Show

mytree = Node Nil 5 Nil

otherthing = Node (mytree) 2 Nil

treesum Nil          = 0
treesum (Node l n r) = treesum l + n + treesum r

howmany Nil          = 0
howmany (Node l n r) = howmany l + 1 + howmany r

howmanyleaves Nil = 1
howmanyleaves (Node l n r) = howmanyleaves l + 0 + howmanyleaves r

howmanydeadends Nil = 0
howmanydeadends (Node Nil _ Nil) = 1
howmanydeadends (Node l n r) = howmanydeadends l + 0 + howmanydeadends r

instance Functor Tree where
    fmap f Nil            = Nil
    fmap f (Node t1 x t2) = Node (fmap f t1) (f x) (fmap f t2)

myOtherTree :: Tree Season
myOtherTree = Node Nil Winter (Node Nil Summer Nil)

-- pattern matching
-- each time the function is called, each line is tried in turn,
-- the first matching left hand side gets to execute its right hand side
mylength []     = 0
mylength (x:xs) = 1 + mylength xs

-- the second line with the (x:xs) construct above means that if a list is passed to mylength
-- the first item of the list is assigned to x, and the rest of the list is assigned to xs

-- you can try the mylength function above by typing this into hugs or ghci:
-- mylength [1,2,3]



-- more pattern matching
-- the underscore matches anything at all
weather        :: Season -> Temp
weather Summer = Hot
weather _      = Cold


-- where clauses introduce definitions local to a function.
ackermann :: Int -> Int
ackermann n = ack n n  -- (Peter's variant of the Ackermann function)
  where
  ack 0 m = m+1
  ack n 0 = ack (n-1) 1
  ack n m = ack (n-1) (ack n (m-1))


-- Typeclasses let you define the same operations on different types
-- the operators equal to ( == ) and not equal to ( /= ) are in the typeclass Eq
instance Eq Temp where
    Cold == Cold = True
    Hot == Hot = True
    _ == _ = False


-- Eq is a built-in typeclass for checking equality, let's define our own typeclass

-- this isn't a very useful typeclass, because it can't sensibly be defined for anything other than Char
-- something like class XML would be more useful, and could be defined for every type
class CharExts a where
    isVowel     :: a -> Bool
    isConsonant :: a -> Bool

instance CharExts Char where
    isVowel a     = elem a "aeiouAEIOU"
    isConsonant a = elem a (filter (not . isVowel) ['A'..'Z'] ++ ['a'..'z'])

-- short demonstration of guards
-- each conditional expression is tried in turn
guard x | x == 0     = 4
        | (x*x) < 64 = 5
        | otherwise  = 6

-- Lazy computation example
-- Evaluation will bounce back and forth between the two infinite lists computing each element.
ones = 1 : ones

odds = 1 : map (+1) evens
evens = map (+1) odds


fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
