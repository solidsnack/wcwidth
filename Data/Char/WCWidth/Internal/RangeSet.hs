module Data.Char.WCWidth.Internal.RangeSet where

import qualified Data.Array.IArray as A
import Data.Array.Unboxed (UArray)
import Data.Char (ord)
import Control.Exception (assert)
import GHC.Exts (IsList(..))

type RawCharRangeSet = [(Int, Int)]

-- | Store end points of the ranges.
newtype CharRangeSet = CharRangeSet{ endPoints :: UArray Int Int }

fromAscList :: RawCharRangeSet -> CharRangeSet
fromAscList = CharRangeSet . listArray . go (-1)
  where go _    []              = []
        go last ((l, u) : rest) = assert (last < l) (l : u + 1 : go u rest)

listArray :: (Num i, A.Ix i, A.IArray a e) => [e] -> a i e
listArray xs = A.listArray (0, fromIntegral (length xs - 1)) xs

binarySearch :: (Ord e, Integral i, A.Ix i, A.IArray a e) => e -> a i e -> Maybe i
binarySearch x arr
  | x < minVal = Nothing
  | otherwise = Just (go l (r + 1))
  -- pre-condition: `x` lies in range [arr!l, arr!r)
  where go l r
          -- given that l <= r
          -- this is equivalent to l == r || l + 1 == r
          | l + 1 >= r = l
          -- invariant: l + 1 < r (l <= r - 2)
          | LT <- cmp = go l m
          | EQ <- cmp = m
          | GT <- cmp = go m r
          -- given that l <= r - 2
          -- invariant: l + 1 <= m <= r - 1
          -- range [l, r) get strictly smaller on recursion
          where m = (l + r) `quot` 2
                cmp = compare x (arr A.! m)
        (l, r) = A.bounds arr
        minVal = arr A.! l

isMemberOf :: Char -> CharRangeSet -> Bool
isMemberOf c = maybe False even . binarySearch (ord c) . endPoints
