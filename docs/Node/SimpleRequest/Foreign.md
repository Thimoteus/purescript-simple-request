## Module Node.SimpleRequest.Foreign

#### `Response`

``` purescript
type Response = { body :: Foreign, statusCode :: Foreign, statusMessage :: Foreign, headersSent :: Foreign, headers :: Foreign, httpVersion :: Foreign, rawHeaders :: Foreign, trailers :: Foreign, rawTrailers :: Foreign }
```

#### `REQUEST`

``` purescript
data REQUEST :: !
```

#### `SRForeign`

``` purescript
newtype SRForeign
  = SRForeign Foreign
```

##### Instances
``` purescript
instance foreignIsOption :: IsOption SRForeign
```

#### `requestImpl`

``` purescript
requestImpl :: forall e. Fn5 Boolean Foreign String (Error -> Eff (request :: REQUEST | e) Unit) (Response -> Eff (request :: REQUEST | e) Unit) (Eff (request :: REQUEST | e) Unit)
```


