## Module Node.SimpleRequest

#### `REQUEST`

``` purescript
type REQUEST = REQUEST
```

#### `Response`

``` purescript
type Response = Response
```

#### `Opts`

``` purescript
type Opts = Options SimpleRequestOptions
```

#### `AffReq`

``` purescript
type AffReq e = Aff (request :: REQUEST | e)
```

#### `SimpleRequestHeader`

``` purescript
data SimpleRequestHeader :: *
```

##### Instances
``` purescript
instance srHeaderIsOption :: IsOption (Options SimpleRequestHeader)
```

#### `SimpleRequestOptions`

``` purescript
data SimpleRequestOptions :: *
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

#### `header2SRHeader`

``` purescript
header2SRHeader :: Header -> Options SimpleRequestHeader
```

Converts a Network.HTTP.Header to an Options SimpleRequestOptions.
Useful if you've defined headers in terms of Network.HTTP.Header.

#### `request`

``` purescript
request :: forall e. Opts -> String -> AffReq e Response
```

#### `get`

``` purescript
get :: forall e. String -> AffReq e Response
```


