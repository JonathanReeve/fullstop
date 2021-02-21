module Tests.NLP.FullStop ( suite ) where

import Data.Char ( isSpace )

import Test.HUnit
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import NLP.FullStop

-- ------------------------------------------------------------
--
-- ------------------------------------------------------------

suite :: Test.Tasty.TestTree
suite =
 testGroup "NLP.FullStop"
  [ testGroup "basic sanity checking"
      [ testProperty "concat (segment s) == id s, modulo whitespace" prop_segment_concat
      ]
  , testGroup "segmentation"
     [ testCaseSegments "simple"  ["Foo.", "Bar."]   "Foo. Bar."
     , testCaseSegments "condense"  ["What?!", "Yeah"]   "What?! Yeah"
     , testCaseSegments "URLs"    ["Check out http://www.example.com.", "OK?"]
                                   "Check out http://www.example.com. OK?"
     , testCaseNoSplit "titles"    "Mr. Doe, Mrs. Durand, St. Orolo and Dr. Singh"
     , testCaseNoSplit "abbreviations"   "e.g., or eg., i.e. or ie. should not be split"
     , testCaseSegments "abbreviations 2"   ["No lie.", "Honestly"] "No lie. Honestly"
     , testCaseNoSplit "initials"  "E. Y. Kow"
     , testCaseNoSplit "initials 2"  "Hello, E. Y. Kow"
     , testCaseSegments "initials counter" [ "E. Y. Kow.", "Hello" ] "E. Y. Kow. Hello"
     , testCaseNoSplit "numbers"   "version 2.3.99.2"
     -- TODO: what's a good way of dealing with ellipsis (...)?
     -- TODO: He said "Foo." Bar (tricky because Foo. "Bar" is legit)
     -- TODO: Very likely to be cases where it's just plain ambiguous
     ]
  ]

testCaseNoSplit d x = testCaseSegments d [x] x
testCaseSegments d xs x = testCase d $ assertEqual "" xs (segment x)

-- TODO: perhaps create a newtype that skews the random generation of tests
-- towards things that look more like text (but not too much, because we still
-- want to make sure we're covering edge-cases)

prop_segment_concat s =
   noWhite s == concatMap noWhite (segment s)
 where
   noWhite = filter (not . isSpace)
