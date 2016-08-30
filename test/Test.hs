
import Test.HUnit

import Data.Access

main = runTestTT tests

tests = test 
  ["Lift works like fmap" ~: "Standard >&> (*2) [1..10]" ~: [2,4..20] ~=? (Standard >&>(*2)) [1..10]
  ,"Explicated Grab works" ~: "grab (*2) 5 == 10" ~: (10 :: Integer) ~=? (*2) ~>> 5
  ,"Explicated Lift works" ~: "((*2).)>&>(+3) $ 5 == 16" ~: (16 :: Integer) ~=? (((*2).)>&>(+3)$5)
  ,"Tuple Accessor working" ~: "Succ three ~>> (1,2,3,4) == 4" ~: 4 ~=? ((undefined :: Succ Three) ~>> (1,2,3,4))
  ]

