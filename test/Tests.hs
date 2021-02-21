import Test.Tasty

import Tests.NLP.FullStop ( suite )

main :: IO ()
main = defaultMain $ testGroup "fullstop"
  [ Tests.NLP.FullStop.suite
  ]
