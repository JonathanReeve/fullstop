-- Copyright (C) 2009 Eric Kow <eric.kow@gmail.com>
--
-- Permission is hereby granted, free of charge, to any person
-- obtaining a copy of this software and associated documentation
-- files (the "Software"), to deal in the Software without
-- restriction, including without limitation the rights to use, copy,
-- modify, merge, publish, distribute, sublicense, and/or sell copies
-- of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
-- BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
-- ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
-- CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

module NLP.FullStop ( segment ) where

import Data.Char
import Data.List
import Data.List.Split

import NLP.FullStop.Ignore ( titles, abbreviations, initials )

-- ------------------------------------------------------------
--
-- ------------------------------------------------------------

-- | 'segment' @s@ splits @s@ into a list of sentences.
--
--   It looks for punctuation characters that indicate an
--   end-of-sentence and tries to ignore some uses of
--   puncuation which do not correspond to ends of sentences
--
--   It's a good idea to view the source code to this module,
--   especially the test suite.
--
--   I imagine this sort of task is actually ambiguous and that
--   you actually won't be able to write an exact segmenter.
--
--   It may be a good idea to go see the literature on how to do
--   segmentation right, maybe implement something which returns
--   the N most probable segmentations instead.
segment :: String -> [String]
segment = map (dropWhile isSpace) . squish . breakup

-- | Helper function to segment
breakup :: String -> [String]
breakup = split
          . condense       -- "huh?!"
          . dropFinalBlank -- strings that end with terminator
          . keepDelimsR    -- we want to preserve terminators
          $ oneOf stopPunctuation

stopPunctuation :: [Char]
stopPunctuation = [ '.', '?', '!' ]

-- ------------------------------------------------------------
-- putting some pieces back together
-- ------------------------------------------------------------

squish = squishBy (\x _ -> any (`isWordSuffixOf` x) abbreviations)
       . squishBy (\_ y -> not (startsWithSpace y))
       . squishBy (\x _ -> looksLikeAnInitial (dropWhile isSpace x))
       . squishBy (\x _ -> any (`isWordSuffixOf` x) (titles ++ initials))
       . squishBy (\x y -> endsWithDigit x  && startsWithDigit y)
 where
  looksLikeAnInitial x =
   case reverse x of
    ('.':i:[])  -> isUpper i
    ('.':i:s:_) -> isSpace s && isUpper i
    _ -> False
  --
  startsW f [] = False
  startsW f (x:_) = f x
  --
  startsWithDigit = startsW isDigit
  startsWithSpace = startsW isSpace
  --
  endsWithDigit xs =
    case reverse xs of
     ('.':x:_) -> isDigit x
     _ -> False

-- | This is *not* (map concat . groupBy f) because the latter
--   just checks equality on the first element of each group.
--   We, on the other hand, want to check by the nearest neighbour
squishBy :: (String -> String-> Bool) -> [String] -> [String]
squishBy _ []     = []
squishBy eq (x:xs) = map concat (helper [] x xs)
 where
  helper acc x0 [] = [assemble acc x0]
  helper acc x0 (x1:xs) =
   if x0 `eq` x1
      then helper (x0:acc) x1 xs
      else assemble acc x0 : helper [] x1 xs
  assemble acc x0 = reverse (x0 : acc)

x  `isWordSuffixOf` y | x `isSuffixOf` y =
 case drop (length x) (reverse y) of
   []    -> True -- x == y
   (z:_) -> not (isAlpha z)
x  `isWordSuffixOf` y = False
