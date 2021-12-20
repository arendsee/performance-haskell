{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( someFunc
    ) where

import qualified Parallel as FP

import System.TimeIt (timeIt)

-- see a very nice tutorial from FPComplete: https://www.fpcomplete.com/haskell/library/vector/

-- Data.Vector offers immutable vectors that behave much like normal haskell
-- lists. They have Functor and Foldable instances and an API similar to what
-- Prelude offers for List.

-- There are six forms of vectors:
--           immutable            mutable                         strict values?  strict spine?
-- boxed     Data.Vector          Data.Vector.Mutable             no              yes
-- storable  Data.Vector.Storable Data.Vector.Storable.Mutable    yes             yes
-- unboxed   Data.Vector.Unboxed  Data.Vector.Unboxed.Mutable     yes             yes
-- generic   Data.Vector.Generic  Data.Vector.Generic.Mutable     either          yes

-- ## boxed. Stores a contiguous array of pointers to normal haskell objects
-- that are allocated on the heap. This allows fast indexing and support for
-- variable sized objects, but also requires dereferencing so is less efficient
-- than storable and unboxed.

-- ## storable. Stores data in a byte array and thus avoids pointer indirection.
-- Stores data in the heap and pins it so the garbage collector cannot move it.
-- This allows the memory to be shared over the C FFI. Elements in a storable
-- vector must be of typeclass Storable.

-- ## unboxed. Stores data in a byte array. Data must be of typeclass Prim and
-- the memory is unpinned, so the garbage collector can move it around, but the
-- data cannot be shared over a C FFI.


-- The `vector` package is build on the `primitive` package, where IO and ST
-- are generalized within the PrimMonad class.

-- # steam fusion. An optimization toggled by -O2 that rewrites vector
-- operations as primitive loops when possible. This avoids large memory
-- allocations.

import           Control.Monad.Primitive     (PrimMonad, PrimState)
import qualified Data.ByteString.Lazy        as L
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U
import           Data.Word                   (Word8)

testUnboxed :: IO ()
testUnboxed = do
    -- Get all of the contents from stdin
    lbs <- L.getContents

    -- Create a new 256-size mutable vector
    -- Fill the vector with zeros
    mutable <- M.replicate 256 0

    -- Add all of the bytes from stdin
    addBytes mutable lbs

    -- Freeze to get an immutable version
    vector <- U.unsafeFreeze mutable

    -- Print the frequency of each byte
    -- In newer vectors: we can use imapM_
    U.zipWithM_ printFreq (U.enumFromTo 0 255) vector

addBytes :: (PrimMonad m, M.MVector v Int)
         => v (PrimState m) Int
         -> L.ByteString
         -> m ()
addBytes v lbs = mapM_ (addByte v) (L.unpack lbs)

addByte :: (PrimMonad m, M.MVector v Int)
        => v (PrimState m) Int
        -> Word8
        -> m ()
addByte v w = do
    -- Read out the old count value
    oldCount <- M.read v index
    -- Write back the updated count value
    M.write v index (oldCount + 1)
  where
    -- Indices in vectors are always Ints. Our bytes come in as Word8, so we
    -- need to convert them.
    index :: Int
    index = fromIntegral w

printFreq :: Int -> Int -> IO ()
printFreq index count = putStrLn $ concat
    [ "Frequency of byte "
    , show index
    , ": "
    , show count
    ]


someFunc = do
  -- testPara
  -- testUnboxed
  -- timeIt . putStrLn . show $ FP.testThreeFibonacci
  -- timeIt . putStrLn . show $ FP.testThreeFibonacciPara
  timeIt . putStrLn . show $ FP.nfibPara
