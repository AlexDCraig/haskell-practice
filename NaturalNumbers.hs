module Nat where

import Prelude hiding (Enum(..), sum)

-- | Representation of the natural numbers.

data Nat = Zero
         | Succ Nat
         deriving (Eq,Show)


-- | Predecessor of a natural number
--   
-- Explanation: Base case - The predecessor to Zero is Zero. 
-- Recursive case - Given a number that must be the successor to some
-- number since it isn't 0, return that number without one Succ 

pred :: Nat -> Nat
pred Zero = Zero
pred (Succ natNum) = natNum

-- | True if the given value is zero.
-- Explanation: If the function is passed the data type Zero, return true
-- if it's passed anything else return false

isZero :: Nat -> Bool
isZero Zero = True
isZero _ = False

-- | Convert a natural number to an integer.
-- Explanation: base case- if its passed the data type Zero, return
-- the integer 0. Otherwise return 1 + (number passed in - 1)
-- to recursively accumulate the number

toInt :: Nat -> Int
toInt Zero = 0
toInt (Succ x) = 1 + toInt x

-- | Add two natural numbers.
-- Explanation: Base case - Zero + any number is that second number.
-- Recursive case - given a number that is by definition the successor
-- to another number and another number to add it to, find the successor
-- to recursively calling the previous number and the same second number
-- until x is 0

add :: Nat -> Nat -> Nat
add Zero x = x
add (Succ x) y = Succ (add x y)


-- | Subtract the second natural number from the first. Return zero
--   if the second number is bigger. *****
-- Explanation - base cases: Zero minus anything is Zero, x minus Zero
-- is x. Recursively call the function using (x-1) and (y-1) until
-- base case is reached

sub :: Nat -> Nat -> Nat
sub Zero x = Zero
sub x Zero = x
sub x y = sub (pred x) (pred y)

-- | Is the left value greater than the right?
-- Explanation: Base case - Any successor to a number is greater than 0.
-- Recursive case- given two successors to another number, recursively call
-- the function with the predecessor number until base case is reached
-- If the first two cases are not ever reached, return false because
-- the numbers are the same

gt :: Nat -> Nat -> Bool
gt (Succ x) Zero = True
gt (Succ x) (Succ y) = gt x y
gt _ _ = False


-- | Multiply two natural numbers.
-- Explanation: Base case- Zero multiplied by anything is Zero. 
-- Recursive case - add the right argument to the  predecessor 
-- of the left argument after recursively calling the multiply
-- function on the predecessor of the left argument and the original
-- right argument

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ x) y = add y (mult x y)


-- | Compute the sum of a list of natural numbers.
-- Explanation: base case- the sum of an empty set is 0.
-- Recursive case - for each element h of the list t, 
-- add h to the current sum of all of the elements

sum :: [Nat] -> Nat
sum [] = Zero
sum (h:t) = add h (sum t)


-- | An infinite list of all of the *odd* natural numbers, in order.
-- Explanation: "one" is the first element and is Succ (Zero),
-- map means apply a function to each element of a list,
-- add is the function we're mapping with two provided as an argument
-- odds is the recursive call w/o a base case to make the list infinite

odds = one : map (add two) odds