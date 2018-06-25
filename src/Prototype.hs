module Prototype where

import Prelude hiding (writeFile)

import Control.Monad.State (evalState, State, get, put)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Aeson
import Data.ByteString.Lazy hiding (pack)
import Data.Monoid ((<>))
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text
import Data.Vector (fromList)
import GHC.TypeLits
import Servant.API

data ApiContext = Context { apiName :: Text, parentName :: Text, parentIsRoot :: Bool, resources :: Set Text }

type TestApi = "Test" :> "Api" :> "Endpoint" :> Get '[JSON] ()
type TestMulti = "a" :> Get '[JSON] ()
            :<|> "b" :> "c" :> Get '[JSON] ()
            :<|> "b" :> "c" :> "d" :> Get '[JSON] ()
            :<|> "b" :> "d" :> "c2" :> Get '[JSON] ()

class HasDeployment api where
  template :: Proxy api -> WriterT [Text] (State ApiContext) [(Text, Value)]

instance (HasDeployment a, HasDeployment b) => HasDeployment (a :<|> b) where
  template Proxy = do
    let
      proxyA = Proxy :: Proxy a
      proxyB = Proxy :: Proxy b
    templateA <- template proxyA
    templateB <- template proxyB
    pure $ templateA <> templateB

instance (KnownSymbol head, HasDeployment tail) => HasDeployment (head :> tail) where
  template Proxy = do
    c@(Context api parent pRoot rs) <- get
    let
      headTemplate = (resourceName, resource)
      newContext = c { parentName = resourceName, parentIsRoot = False }
      resourceName = currentPathPart <> "Resource"
      currentPathPart = pack $ symbolVal pathPartSymbol
      pathPartSymbol = Proxy :: Proxy head
      remainingPath = Proxy :: Proxy tail
      rootResource = getAtt api "RootResourceId"
      resource = object [
        "Type" .= String "AWS::ApiGateway::Resource",
        "Properties" .= object [
          "ParentId" .= if pRoot then rootResource else ref parent,
          "PathPart" .= currentPathPart,
          "RestApiId" .= ref api
         ]
        ]
    put newContext
    tailTemplate <- template remainingPath
    if (S.member resourceName rs)
      then do
        put newContext
        pure tailTemplate
      else do
        put (newContext { resources = S.insert resourceName rs })
        pure $ headTemplate : tailTemplate

ref :: Text -> Value
ref resource = object ["Ref" .= String resource]

getAtt :: Text -> Text -> Value
getAtt resource prop = object ["Fn::GetAtt" .= (Array $ fromList [String resource, String prop])]

instance HasDeployment (Verb 'GET status c m) where
  template _ = do
    (Context api parent _ _)  <- get
    let
      name = parent <> "GET"
      method = object [
        "Type" .= String "AWS::ApiGateway::Method",
        "Properties" .= object [
          "HttpMethod" .= String "GET",
          "RestApiId" .= ref api,
          "ResourceId" .= ref parent,
          "MethodResponses" .= (fromList $ [object [
            "StatusCode" .= Number 200
          ]]),
          "AuthorizationType" .= String "NONE",
          "Integration" .= object [
            "IntegrationResponses" .= (fromList $ [object ["StatusCode" .= Number 200]]),
            "RequestTemplates" .= object ["application/json" .= String "{ statusCode: 200 }"],
            "Type" .= String "MOCK"
          ]
        ]
       ]
    tell [name]
    pure [(name, method)]

deploy :: (HasDeployment api) => Text -> Proxy api -> ByteString
deploy name p@Proxy = encode $ object [ "Resources" .= (object $ baseTemplate <> templateBody) ]
  where
    baseTemplate = restApiTemplate apiName deps
    (templateBody, deps) = (flip evalState) ctx $ runWriterT program
    ctx = Context apiName apiName True S.empty
    apiName = name <> "Api"
    program = template p

mkDeployFile = writeFile "./deploy.json" $ deploy "Test" (Proxy :: Proxy TestMulti)

restApiTemplate :: Text -> [Text] -> [(Text, Value)]
restApiTemplate name dependencies = do
  [
    name .= object [
      "Type" .= String "AWS::ApiGateway::RestApi",
      "Properties" .= object [
        "Name" .= name
      ]
    ],
    (name <> "Deployment") .= object [
      "Type" .= String "AWS::ApiGateway::Deployment",
      "DependsOn" .= (fromList $ String <$> dependencies),
      "Properties" .= object [
        "RestApiId" .= ref name,
        "StageName" .= String "default"
      ]
    ]
   ]


