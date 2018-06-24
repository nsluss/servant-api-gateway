# servant-api-gateway

This is currently a very rough cut and not usable for anything other than proof of concept.

All we can do so far is generate a cloud formation template with mock endpoints for get requests.

### example
``` haskell
type TestApi = "Test" :> "Api" :> "Endpoint" :> Get '[JSON] ()

deploy "Test" (Proxy :: Proxy TestApi)
```
