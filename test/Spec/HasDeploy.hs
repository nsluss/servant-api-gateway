module Spec.HasDeploy where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), Assertion)

import Data.Tree (Tree(..))
import Data.Proxy
import GHC.TypeLits
import Servant.API ((:>), (:<|>))

import Lib

(===) :: (Eq a, Show a) => a -> a -> Assertion
(===) = (@?=)
infixl 1 ===

type SimplePathPart = "test" :> ()
type TwoPartPath = "test" :> "api" :> ()
type MultiRoutePath = "test" :> "api" :> ()
                 :<|> "second" :> "route" :> ()

tests :: TestTree
tests = testGroup "HasDeploy" $ [
    testCase "tests are set up properly" $ do
      let
        actual = True
        expected = True
      actual === expected
  , testGroup "(:>)" [
      testCase "registers a path part" $ do
        let
          simpleProxy = Proxy :: Proxy SimplePathPart
          actual = deploySpec simpleProxy
          expected = [ Node "test" [] ]
        actual === expected
    , testCase "registers a path with multiple parts" $ do
        let
          proxy = Proxy :: Proxy TwoPartPath
          actual = deploySpec proxy
          expected = [ Node "test" [ Node "api" [] ] ]
        actual === expected
  ]
  , testGroup "Unit" [
      testCase "registers nothing" $ do
        let
          proxy = Proxy :: Proxy ()
          actual = deploySpec proxy
          expected = []
        actual === expected
  ]
  , testGroup "(:<|>)" [
      testCase "registers a path with multiple routes" $ do
        let
          proxy = Proxy :: Proxy MultiRoutePath
          actual = deploySpec proxy
          route1 = Node "test" [ Node "api" [] ]
          route2 = Node "second" [ Node "route" [] ]
          expected = [route1, route2]
        actual === expected
    ]
  ]
