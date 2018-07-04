module Lib where

import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Text (Text, pack, unpack)
import Data.Tree (Tree(..), Forest, drawTree)
import GHC.TypeLits
import Servant.API ((:>), (:<|>))

someFunc :: IO ()
someFunc = print "someFunc"

class HasDeploy a where
  deploySpec :: Proxy a -> Forest Text

instance (KnownSymbol head, HasDeploy tail) => HasDeploy (head :> tail) where
  deploySpec _ = [ Node headName $ deploySpec tailProxy ]
    where
      headName = pack $ symbolVal (Proxy :: Proxy head)
      tailProxy = Proxy :: Proxy tail

instance (HasDeploy l, HasDeploy r) => HasDeploy (l :<|> r) where
  deploySpec _ = leftSpec <> rightSpec
    where
      leftSpec = deploySpec lProxy
      rightSpec = deploySpec rProxy
      lProxy = Proxy :: Proxy l
      rProxy = Proxy :: Proxy r

instance HasDeploy () where
  deploySpec _ = []

type Test = "test" :> "api" :> ()
       :<|> "route" :> ()
       :<|> "sub" :> SubAPI
type SubAPI = "thing" :> ()
         :<|> "other" :> ()

testSpec = deploySpec (Proxy :: Proxy Test)

prettyPrint :: Forest Text -> IO ()
prettyPrint forest = putStrLn $ drawTree tree
  where
    tree = unpack <$> Node "root" forest

