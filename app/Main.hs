module Main where

import Lib

main :: IO ()
main = do
  let a = F1 "first"  :: Foo String String String
      b = F1 "second" :: Foo String String String
  print $ eq a a
  print $ eq a b
