## go kin-openapiのopenapi3filterの実装を把握したい

とりあえず呼び出し関係から

- [S] AuthenticationInput

    - [M] NewError(err error) error

- [S] RequestError <- error
- [S] ResponseError <- error
- [S] Validator

    - [F] NewValidator(...) *Validator
    - [M] Middlware(http.Handler) http.Handler

- [I] (responseWriter)

    - [M] flushBodyContents() error
    - [M] statusCode() int
    - [M] bodyContents() []byte

- [S] (warnResponseWriter) <- responseWriter>

    - [M] Write([]byte) (int, error)
    - [M] WriteHeader(status int)
    - [M] Header() http.Header
    - [M] Flush()
    - [M] statusCode() int
    - [M] bodyContents() int

- [S] (strictResponseWrapper) <- responseWriter

    - [M] Write([]byte) (int, error)
    - [M] WriteHeader(status int)
    - [M] Header() http.Header
    - [M] statusCode() int
    - [M] bodyContents() int

- [S] Options
- [S] ParseError <- error

    - [M] RootCause()
    - [M] Path() []interface{}

- [F] ValidateRequest(context.Context, *RequestValidationInput) error
- [F] ValidateParameter(context.Context, *RequestValidationInput, *openapi3.Parameter) error
- [F] ValidateRequestBody(context.Context, *RequestValidationInput, *openapi3.RequestBody) error
- [F] ValidateSecurityRequirements(context.Context, *RequestValidationInput, openapi3.SecurityRequirements) error
- [F] ValidateResponse(context.Context, *ResponseValidationInput) error
- [S] ValidationError <- error>

    - [M] StatusCode() int

- [S] ValidationErrorEncoder

    - [M] Encode(context.Context, err erro, w http.ResponseWriter)

- [S] ValidationHandler

    - [M] Load() error
    - [M] Middleware(http.Handler) http.Handler


hmm

- ValidationErrorEncoderにいい感じエラーを渡すと良い？
    - しかし、これはそのままResponseWriterに出力してしまうんだな。。
- ValidationHandlerは便利だけどlegacyrouterに依存している感じ
    - あと、response validationがついていない


```go
// Router helps link http.Request.s and an OpenAPIv3 spec
type Router interface {
	// FindRoute matches an HTTP request with the operation it resolves to.
	// Hosts are matched from the OpenAPIv3 servers key.
	//
	// If you experience ErrPathNotFound and have localhost hosts specified as your servers,
	// turning these server URLs as relative (leaving only the path) should resolve this.
	//
	// See openapi3filter for example uses with request and response validation.
	FindRoute(req *http.Request) (route *Route, pathParams map[string]string, err error)
}
```


```go
    ctx := context.Background()
    statusCode := 200
    // requestValidationInput?
reqInput := &openapi3filter.RequestValidationInput{
    Req: req,
    Route: ...,
}
resInput := &openapi3filter.ResponseValidationInput{
			RequestValidationInput: reqInput, 
			Status:                 statusCode,
			Header: http.Header{
				headerCT: []string{
					resp.ContentType,
				},
			},
            // body? ioutil.NopCloser(bytes.NewBuffer(wr.bodyContents())),
		}
    err := ValidateResponse(ctx, resInput)
    fmt.Println(err)    
}
```


hmm


```go
func main(){
    ctx := context.Background()
    httpReq, err := http.NewRequest(req.Method, req.URL, marshalReader(req.Body))
    // Find route
		route, pathParams, err := router.FindRoute(httpReq)
		require.NoError(t, err)

// Now, repeat the above two test cases using a custom parameter decoder.
	customDecoder := func(param *openapi3.Parameter, values []string) (interface{}, *openapi3.Schema, error) {
		var value interface{}
		err := json.Unmarshal([]byte(values[0]), &value)
		schema := param.Content.Get("application/something_funny").Schema.Value
		return value, schema, err
	}

requestValidationInput := &RequestValidationInput{
			Request:      httpReq,
			PathParams:   pathParams,
			Route:        route,
			ParamDecoder: decoder,
		}
    input := &openapi3filter.ResponseValidationInput{
			RequestValidationInput: requestValidationInput,
			Status:                 resp.Status,
			Header: http.Header{
				headerCT: []string{
					resp.ContentType,
				},
			},
		}
    err := ValidateResponse(ctx, input)
    fmt.Println(err)    
}
```