## Module Node.SimpleRequest

#### `SimpleRequestOptions`

``` purescript
data SimpleRequestOptions :: *
```

#### `SimpleRequestHeader`

``` purescript
data SimpleRequestHeader :: *
```

##### Instances
``` purescript
instance srHeaderIsOption :: IsOption (Options SimpleRequestHeader)
```

#### `REQUEST`

``` purescript
data REQUEST :: !
```

#### `Opts`

``` purescript
type Opts = Options SimpleRequestOptions
```

#### `AffReq`

``` purescript
type AffReq e = Aff (request :: REQUEST | e)
```

#### `Response`

``` purescript
type Response = { body :: Foreign, statusCode :: Foreign, statusMessage :: Foreign, headersSent :: Foreign, headers :: Foreign, httpVersion :: Foreign, rawHeaders :: Foreign, trailers :: Foreign, rawTrailers :: Foreign }
```

#### `srHeader`

``` purescript
srHeader :: HeaderHead -> Option SimpleRequestHeader String
```

Takes a HeaderHead and gives a value you can use as a header object.
For example: 
```purescript
reqHeader :: Options SimpleRequestHeader
reqHeader = srHeader ContentType := "application/x-www-form-urlencoded"
         <> srHeader ContentLength := "20"
```

#### `request`

``` purescript
request :: forall e. Opts -> String -> AffReq e Response
```

#### `get`

``` purescript
get :: forall e. String -> AffReq e Response
```


