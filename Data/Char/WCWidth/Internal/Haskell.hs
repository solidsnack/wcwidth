module Data.Char.WCWidth.Internal.Haskell (wcwidth) where

import Data.Char
import Data.Char.WCWidth.Internal.RangeSet
import Data.Char.WCWidth.Internal.Data

wcwidth :: Char -> Int
wcwidth c
  | c `isMemberOf` basicZero = 0
  | c `isMemberOf` ctrlChars = -1
  | c `isMemberOf` wideEastAsian = 2
  | c `isMemberOf` zeroWidth = 0
  | otherwise = 1
