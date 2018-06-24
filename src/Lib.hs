module Lib where

import Prelude hiding (writeFile)

import Data.Aeson
import Data.ByteString.Lazy hiding (pack)
import Data.Monoid ((<>))
import Data.Proxy
import Data.Text
import Data.Vector (fromList)
import GHC.TypeLits
import Servant.API

someFunc :: IO ()
someFunc = print "someFunc"

data ApiContext = Context { apiName :: Text, parentName :: Text, parentIsRoot :: Bool }

data ApiPart =
  Resource {
    pathPart :: Text
  , name :: Text
  } | End

type TestApi = "Test" :> "Api" :> "Endpoint" :> Get '[JSON] ()

class HasDeployment api where
  template :: Proxy api -> ApiContext -> [(Text, Value)]

instance (KnownSymbol head, HasDeployment rest) => HasDeployment (head :> rest) where
  template Proxy c@(Context api parent pRoot) = (resourceName, resource) : template remainingPath newContext
    where
      newContext = c { parentName = resourceName, parentIsRoot = False }
      resourceName = currentPathPart <> "Resource"
      currentPathPart = pack $ symbolVal pathPartSymbol
      pathPartSymbol = Proxy :: Proxy head
      remainingPath = Proxy :: Proxy rest
      rootResource = getAtt api "RootResourceId"
      resource = object [
        "Type" .= String "AWS::ApiGateway::Resource",
        "Properties" .= object [
          "ParentId" .= if pRoot then rootResource else ref parent,
          "PathPart" .= currentPathPart,
          "RestApiId" .= ref api
        ]
       ]

ref :: Text -> Value
ref resource = object ["Ref" .= String resource]

getAtt :: Text -> Text -> Value
getAtt resource prop = object ["Fn::GetAtt" .= (Array $ fromList [String resource, String prop])]

instance HasDeployment () where
  template _ _ = []

instance HasDeployment (Verb 'GET 200 c d) where
  template _ (Context api parent _) = [(name, method)]
    where
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

deploy :: (HasDeployment api) => Text -> Proxy api -> ByteString
deploy name p@Proxy = encode $ object [
    "Resources" .= (object $ (restApiTemplate apiName) <> template p ctx)
  ]
    where
        apiName = name <> "Api"
        ctx = Context apiName apiName True

mkDeployFile = writeFile "./deploy.json" $ deploy "Test" (Proxy :: Proxy TestApi)

restApiTemplate :: Text -> [(Text, Value)]
restApiTemplate name = [
  name .= object [
    "Type" .= String "AWS::ApiGateway::RestApi",
    "Properties" .= object [
      "Name" .= name
    ]
  ],
  (name <> "Deployment") .= object [
    "Type" .= String "AWS::ApiGateway::Deployment",
    "DependsOn" .= (fromList [String "EndpointResourceGET"]),
    "Properties" .= object [
      "RestApiId" .= ref name,
      "StageName" .= String "default"
    ]
  ]
 ]



