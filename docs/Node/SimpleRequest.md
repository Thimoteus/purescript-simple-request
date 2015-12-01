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

#### `SimpleRequestOptions`

``` purescript
data SimpleRequestOptions :: *
```

#### `SRHeaderOptions`

``` purescript
newtype SRHeaderOptions
  = SRHeaderOptions (Options SimpleRequestHeader)
```

##### Instances
``` purescript
IsOption SRHeaderOptions
```

#### `runSRHeaderOptions`

``` purescript
runSRHeaderOptions :: SRHeaderOptions -> Options SimpleRequestHeader
```

#### `Verb`

``` purescript
data Verb
  = DELETE
  | HEAD
  | GET
  | OPTIONS
  | PATCH
  | POST
  | PUT
```

##### Instances
``` purescript
Generic Verb
Show Verb
IsOption Verb
```

#### `host`

``` purescript
host :: Option SimpleRequestOptions String
```

Options values as specified by [http.request](https://nodejs.org/api/http.html#http_http_request_options_callback).

#### `hostname`

``` purescript
hostname :: Option SimpleRequestOptions String
```

#### `port`

``` purescript
port :: Option SimpleRequestOptions Int
```

#### `localAddress`

``` purescript
localAddress :: Option SimpleRequestOptions String
```

#### `socketPath`

``` purescript
socketPath :: Option SimpleRequestOptions String
```

#### `method`

``` purescript
method :: Option SimpleRequestOptions Verb
```

#### `path`

``` purescript
path :: Option SimpleRequestOptions String
```

#### `headers`

``` purescript
headers :: Option SimpleRequestOptions SRHeaderOptions
```

#### `auth`

``` purescript
auth :: Option SimpleRequestOptions String
```

#### `keepAlive`

``` purescript
keepAlive :: Option SimpleRequestOptions Boolean
```

#### `keepAliveMsecs`

``` purescript
keepAliveMsecs :: Option SimpleRequestOptions Int
```

#### `srHeader`

``` purescript
srHeader :: HeaderHead -> Option SimpleRequestHeader String
```

Takes a HeaderHead and gives a value you can use as a header object.
For example:
```purescript
reqHeader :: SRHeaderOptions
reqHeader = SRHeaderOptions (srHeader ContentType := "application/x-www-form-urlencoded"
         <> srHeader ContentLength := "20")
```

#### `srHeaderOpts`

``` purescript
srHeaderOpts :: Array (Tuple HeaderHead String) -> SRHeaderOptions
```

Takes an array of (HTTP.HeaderHead, String) tuples and creates an SRHeaderOptions value.

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


