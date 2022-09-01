import Test.Hspec
import Test.Hspec.QuickCheck

import Data.Char (ord)
import Control.Monad (forM_)
import Data.Array.IArray (bounds, (!))

import Data.Char.WCWidth.Internal.RangeSet
import Data.Char.WCWidth.Internal.Data

main :: IO ()
main = hspec $ do
  describe "binarySearch: gets the proper index" $ do
    forM_ setPairs $ \(name, _, CharRangeSet set) -> do
      prop name $ \x -> case binarySearch x set of
        Nothing -> x < set ! 0
        Just k -> set ! k <= x && (k == snd (bounds set) || x < set ! (k + 1))
  describe "CharRangeSet: agrees with linear search" $ do
    forM_ setPairs $ \(name, rawSet, set) -> do
      prop name $ \c -> ord c `trivialIsMemberOf` rawSet == c `isMemberOf` set

trivialIsMemberOf :: Ord a => a -> [(a, a)] -> Bool
trivialIsMemberOf x = any (\(l, u) -> l <= x && x <= u)

setPairs :: [(String, RawCharRangeSet, CharRangeSet)]
setPairs =
  [ ("Basic Zero", basicZeroRaw, basicZero)
  , ("Ctrl Chars", ctrlCharsRaw, ctrlChars)
  , ("Wide East Asian", wideEastAsianRaw, wideEastAsian)
  , ("Zero Width", zeroWidthRaw, zeroWidth)
  ]
