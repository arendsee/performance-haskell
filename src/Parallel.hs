module Parallel
  ( twoMeans
  , twoMeansPar
  , testPara
  -- , testThreeFibonacci
  -- , testThreeFibonacciPara
  , nfib
  , nfibPara
  ) where

import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (Eval, Strategy, rpar, rseq, runEval)
import Control.DeepSeq (force, NFData)
import System.TimeIt (timeIt)

-- Control.Parallel only exports these two functions:
-- par  :: a -> b -> b
-- pseq :: a -> b -> b
--
-- par indicates to GHC that is evaluating a in parallel with b is permitted.
-- The runtime decides whether to actually spawn a new thread.
--   a `par` b  ===  b
-- `par` can be used when `a` will likely be used later. `a` should not be a
-- trivial computation or the constant cause of spawning can outweigh the
-- benefit.
--
-- pseq is semantically equivalent to seq, but is strict only in its first
-- argument. This prevents the compiler from rearranging `seq a b` to `seq b
-- (seq a b)` or semantically identical expression that could interfere with
-- parallelism. `b` may already be in the process of being solved in another
-- thread, so we should not have to wait for it.

-- Weak Head Normal Form
--  * a constructor `Just (1 + 1)`
--  * a partially-applied function `(x) (2 + 1)`
--  * a lambda `\x -> 1 + 2`

{-
foldl :: (b -> a -> b) -> b -> [a] -> b   -- simplifying to just lists
foldl _ b [] = b
foldl f b (x:xs) = foldl f (f b x) xs


-- foldr runs out of memory, heap space, as its thunk grows
foldl (+) 0 [1,2,3,4,5]
= foldl (+) (0 + 1) [2,3,4,5]  -- this is NOT in WHNF since it is fully applied
= foldl (+) ((0 + 1) + 2) [3,4,5]
= foldl (+) (((0 + 1) + 2) + 3) [4,5]
= foldl (+) ((((0 + 1) + 2) + 3) + 4) [5]
= foldl (+) (((((0 + 1) + 2) + 3) + 4) + 5) []
= (((((0 + 1) + 2) + 3) + 4) + 5)
= ((((1 + 2) + 3) + 4) + 5)
= (((3 + 3) + 4) + 5)
= ((6 + 4) + 5)
= (10 + 5)
= 15
-}

-- In contrast:

{-
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ b [] = b
foldr f b (x:xs) = f x (foldr b xs)

foldr (+) 0 [1,2,3,4,5]
= 1 + foldr (+) 0 [2,3,4,5]
= 1 + 2 + foldr (+) 0 [3,4,5]
= 3 + 3 + foldr (+) 0 [4,5]
= 6 + 4 + foldr (+) 0 [5]
= 10 + 5 + foldr (+) 0 []
= 15 + 0
= 15


--- foldr blows the stack, it keeps pushing things on until the stack is exhausted
foldr f 0 [1,2,3,4,5]
= f 1 (foldr (+) 0 [2,3,4,5])
= f 1 (f 2 (foldr (+) 0 [3,4,5]))
= f 1 (f 2 ( f 3 (foldr (+) 0 [4,5])))
= f 1 (f 2 ( f 3 ( f 4 (foldr (+) 0 [5]))))
= f 1 (f 2 ( f 3 ( f 4 ( f 5 (foldr (+) 0 [])))))
= f 1 (f 2 ( f 3 ( f 4 ( f 5 0))))
-}

-- So neither foldr NOR foldl are good. What is the solution, use seq.

{-
foldl' :: (a -> b -> a) -> b -> [a] - b
foldl' _ b [] = b
foldl' f b (x:xs) =
  let b' = f b x
  in seq b' $ foldl' f b' xs
-}


{-
-- NO! This is in fact a left fold.
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ b [] = b
foldr' f b (x:xs) = let b' = f x b in (foldr b xs)
-}

-- We can fix the think problem

-- FYI: seq does not fully force evaluation. It only requires that `a` not be
-- bottom. To prove this it is only required to evalute to Weak Head Normal
-- Form -- that evaluates to the top of a data constructor. For instance `seq
-- (take 2 [1,2,3])` would evaluate to `(:) 1 [2]`. The `:` is the top-level
-- lambda expression. `seq` does NOT force the entire evaluation down to the normal
-- form `[1,2]`.

-- `seq x y` means that whenever `y` is evaluated to WHNF, `x` is as well. It
-- does not force an evaluation order, it does not mean `x` must be evaluated
-- first.


-- import Control.Parallel.Strategies (rnf)
-- -- Strategies are a way to separate the parallelism from the algorithm
-- --  O'Reilly book as a nice description: https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/ch03.html

-- Actually, I've bought that book and will return to parallelism when it
-- arrives. For now, I'll segue to unboxed types.

parSort :: (Ord a) => [a] -> [a]
parSort (x:xs)    = greater `par` (lesser `pseq`
                                         (lesser ++ x:greater))
    where lesser  = parSort [y | y <- xs, y <  x]
          greater = parSort [y | y <- xs, y >= x]
parSort _ = []

forceList :: [a] -> ()
forceList (x:xs) = x `pseq` forceList xs
forceList _      = ()

twoMeans :: Int -> [Double]
twoMeans n = [a, b, c]
  where
  a = mean [1..n]
  b = mean [1..(n+1)]
  c = mean [1..(n+2)]

twoMeansPar :: Int -> [Double]
twoMeansPar n = a `par` b `par` c `par` [a, b, c]
  where
  a = mean [1..n]
  b = mean [1..(n+1)]
  c = mean [1..(n+2)]

mean :: [Int] -> Double
mean x = fromIntegral (sum x) / fromIntegral (length x) 


prodsum :: Integer -> Integer
prodsum i = sum $ [x * y * z | x <- [1..i], y <- [1..i], z <- [1..i]]

fibonacci :: Integer -> Integer
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci x = fibonacci (x-1) + fibonacci (x-2)

parMap :: NFData b => (a -> b) -> [a] -> Eval [b]
parMap f [] = return []
parMap f (a:as) = do
  b <- rpar (f a)
  bs <- parMap f as
  return (b:bs)

nfibPara = runEval . parTriple $ (fibonacci 34, fibonacci 35, fibonacci 36)

nfib = (fibonacci 34, fibonacci 35, fibonacci 36)


parTriple :: Strategy (a, b, c) 
parTriple (a, b, c) = do
  a' <- rpar a
  b' <- rpar b
  c' <- rpar c
  return (a', b', c')


-- -- no speed up from parallelization? why?
-- testThreeFibonacciPara = runEval $ do
--   a <- rpar $ force $ prodsum [1..421]
--   b <- rpar $ force $ prodsum [2..422]
--   c <- rpar $ force $ prodsum [3..423]
--   rseq a
--   rseq b
--   rseq c
--   return ("parallel", a, b, c)
--
-- testThreeFibonacci =
--   let
--     a = prodsum [4..424]
--     b = prodsum [5..425]
--     c = prodsum [6..426]
--   in ("sequential", a, b, c)

testPara = do
  timeIt . putStrLn . show $ twoMeansPar 1234567
  timeIt . putStrLn . show $ twoMeans 1234567
  timeIt . putStrLn . show $ twoMeansPar 1234568
  timeIt . putStrLn . show $ twoMeans 1234568
  -- putStrLn $ show $ length $ parSort [1..134]
