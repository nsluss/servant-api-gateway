import Test.Tasty (TestTree, testGroup, defaultMain)
import qualified Spec.HasDeploy as Deploy

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "servant-api-gateway" [
    Deploy.tests
  ]
