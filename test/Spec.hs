{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import Test.HUnit

import Data.Hjq.Parser

main :: IO ()
main = do
  _ <- runTestTT $ TestList
    [ jqFilterParserTest
    , jqFilterParserSpacesTest
    ]
  return ()

jqFilterParserTest :: Test
jqFilterParserTest = TestList
  [ "jqFilterParser test 1" ~: parseJqFilter "." ~?= Right JqNil
  , "jqFilterParser test 2" ~: parseJqFilter ".[0]" ~?= Right (JqIndex 0 JqNil)
  , "jqFilterParser test 3" ~: parseJqFilter ".fieldName" ~?= Right (JqField "fieldName" JqNil)
  , "jqFilterParser test 4" ~: parseJqFilter ".[0].fieldName" ~?= Right (JqIndex 0 (JqField "fieldName" JqNil))
  , "jqFilterParser test 5" ~: parseJqFilter ".fieldName[0]" ~?= Right (JqField "fieldName" (JqIndex 0 JqNil))
  ]

jqFilterParserSpacesTest :: Test
jqFilterParserSpacesTest = TestList
  [ "jqFilterParser spaces test 1" ~: parseJqFilter " . " ~?= Right JqNil
  , "jqFilterParser spaces test 2" ~: parseJqFilter " . [ 0 ] " ~?= Right (JqIndex 0 JqNil)
  , "jqFilterParser spaces test 3" ~: parseJqFilter " . fieldName " ~?= Right (JqField "fieldName" JqNil)
  , "jqFilterParser spaces test 4" ~: parseJqFilter " . [ 0 ] . fieldName " ~?= Right (JqIndex 0 (JqField "fieldName" JqNil))
  , "jqFilterParser spaces test 5" ~: parseJqFilter " . fieldName [ 0 ] " ~?= Right (JqField "fieldName" (JqIndex 0 JqNil))
  ]
