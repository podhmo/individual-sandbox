## go hatenaのインターンの課題のディレクトリ構造などを把握してみる

2つあるけれど。どちらも同じような構造何だろうか？

- [hatena/go-Intern-Diary: はてなインターン2018 課題アプリケーションひな形](https://github.com/hatena/go-Intern-Diary "hatena/go-Intern-Diary: はてなインターン2018 課題アプリケーションひな形")
- [hatena/go-Intern-Bookmark: はてなインターン2018 サンプルアプリケーション](https://github.com/hatena/go-Intern-Bookmark "hatena/go-Intern-Bookmark: はてなインターン2018 サンプルアプリケーション")

go-Intern-Diaryは課題でgo-Internn-Bookmarkは産婦要るアプリなのでDiaryの方はコードが無いかも？

### ディレクトリ構造

- config
- (db)
- (loader)
- model
- repository
- resolver
- service
- (templates)
- titleFetcher
- web

@: configは普通にconfigだけ
@: migrationとかはどうする -> dbにsqlがある
@: tableはどこに書く？ -> model
@: dockerは何を動かしている？ -> 全部。app,db,node
@: mainのところは？ main,run,graceful(gracefulはgoroutine,signalを受け取ってserverをshutdown)
@: appからの依存の構築はどうしている？ -> config.Load(); あとは個別にrepository.New()とservice.NewApp()。直接configは渡さない
@: modelにはtableにmappingされるものとerror
@: repositoryのinterfaceの引数には名前を付けている
@: しっかりとmodel.NotFoundErrorの値をprivateなエラー名として登録している
@: get,list,ByIDと言うような名前
@: repository自体はgo-sql-driver/mysqlとかimportしている
@: repositoryは1つの巨大なインターフェイス
@: [cespare/reflex: Run a command when files change](https://github.com/cespare/reflex "cespare/reflex: Run a command when files change")で監視している
@: serviceはBookmarkAppというinterface
@: repositoryと同名のメソッドも持っている。おそらく、webではrepoが一切出ない。はず。
@: そういえば、logは一切無い？
@: 基本的には全部interfaceで公開して、実装はprivate
@: testでtestifyを使っている。serviceだけテストをしておけばという感じではある
@: serviceはrepositoryとtitleFetcherにだけ依存している -> CLIの一部として取り出せそう
@: loggingのmiddlewareは自作のちょっとした関数。logは素直にstdlibのもの。
@: middlewareはhttp.Handler -> http.Handlerという関数
@: templatesのlooadsなどはinitでやっている。これはconfによって変わるとか無いので正しいと思う。
@: serverはappだけに依存している。
@: 各handlerの登録はmiddlewareをくっつけて登録するinternalな関数を作ってそこで登録
@: nosurfはCSRF対策用のやつっぽい
@: 各handlerはprivate method。正確に言うとhandlerを生成する関数。
@: 内部のerrorは使わず自前でerrorをhttp.Errorで書き出している
@: loaderはgraphql用のやつ -> graph-gophersというパッケージがある
@: 何のpackageに依存している？
@: けっこう素直に、interfaceをexport, 実装はprivateに

わかっていないもの

- reflex, opentracing, nosurf , go-assets

便利そうなもの

github.com/davecgh/go-spew
github.com/justinas/nosurf
github.com/opentracing/opentracing-go
github.com/pmezard/go-difflib

### dependencies

```console
$ git grep github.com | grep -P -o 'github.com[a-zA-Z\-/]+' | grep -v hatena | sort -u
github.com/cespare/reflex
github.com/davecgh/go-spew
github.com/dimfeld/httptreemux
github.com/golang/dep/cmd/dep
github.com/go-sql-driver/mysql
github.com/graph-gophers/dataloader
github.com/graph-gophers/graphql-go
github.com/graph-gophers/graphql-go/relay
github.com/jessevdk/go-assets
github.com/jessevdk/go-assets-builder
github.com/jmoiron/sqlx
github.com/justinas/nosurf
github.com/opentracing/opentracing-go
github.com/pmezard/go-difflib
github.com/stretchr/testify
github.com/stretchr/testify/assert
```

## go go-swaggerの最新版を試す

- 過去の独自のコードになれるの本当に良くないよなー。

https://goswagger.io/tutorial/todo-list.html

しっかりとgo-swaggerの今の姿を確認するべき


```makefile
gen:
	swagger generate server -A todo-list -f ./swagger.yml
run:
	go run cmd/todo-list-server/main.go --port 4444
client:
	http :4444/
setup:
	go get -v github.com/go-openapi/loads
	go get -v github.com/go-openapi/runtime
	go get -v github.com/docker/go-units
	go get -v github.com/go-openapi/validate
	go get -v github.com/jessevdk/go-flags
```

```console
$ http :4444/
http :4444/
HTTP/1.1 501 Not Implemented
Content-Length: 51
Content-Type: application/io.goswagger.examples.todo-list.v1+json
Date: Fri, 19 Oct 2018 22:48:02 GMT

"operation todos.Get has not yet been implemented"
```

diff

```diff
diff --git a/restapi/configure_todo_list.go b/restapi/configure_todo_list.go
index f0ea162..3c570c3 100644
--- a/restapi/configure_todo_list.go
+++ b/restapi/configure_todo_list.go
@@ -4,12 +4,15 @@ package restapi
 
 import (
 	"crypto/tls"
+	"log"
 	"net/http"
 
 	errors "github.com/go-openapi/errors"
 	runtime "github.com/go-openapi/runtime"
 	middleware "github.com/go-openapi/runtime/middleware"
+	"github.com/go-openapi/swag"
 
+	"github.com/podhmo/examples/go-swagger/tutorial/models"
 	"github.com/podhmo/examples/go-swagger/tutorial/restapi/operations"
 	"github.com/podhmo/examples/go-swagger/tutorial/restapi/operations/todos"
 )
@@ -28,14 +31,17 @@ func configureAPI(api *operations.TodoListAPI) http.Handler {
 	// Expected interface func(string, ...interface{})
 	//
 	// Example:
-	// api.Logger = log.Printf
+	api.Logger = log.Printf
 
 	api.JSONConsumer = runtime.JSONConsumer()
 
 	api.JSONProducer = runtime.JSONProducer()
 
 	api.TodosGetHandler = todos.GetHandlerFunc(func(params todos.GetParams) middleware.Responder {
-		return middleware.NotImplemented("operation todos.Get has not yet been implemented")
+		payload := []*models.Item{
+			&models.Item{Completed: true, Description: swag.String("run server"), ID: 1},
+		}
+		return todos.NewGetOK().WithPayload(payload)
 	})
 
 	api.ServerShutdown = func() {}

```

```console
$ http :4444/
HTTP/1.1 200 OK
Content-Length: 55
Content-Type: application/io.goswagger.examples.todo-list.v1+json
Date: Fri, 19 Oct 2018 23:05:02 GMT

[
    {
        "completed": true,
        "description": "run server",
        "id": 1
    }
]

```

### debugを有効に

```go

func DebugEnabled() bool {
	return os.Getenv("SWAGGER_DEBUG") != "" || os.Getenv("DEBUG") != ""
}
```

```console
$ DEBUG=1 make run
```

### error responseとか

自分で作成するの？

```go
// Responder is an interface for types to implement
// when they want to be considered for writing HTTP responses
type Responder interface {
	WriteResponse(http.ResponseWriter, runtime.Producer)
}

// ResponderFunc wraps a func as a Responder interface
type ResponderFunc func(http.ResponseWriter, runtime.Producer)

// WriteResponse writes to the response
func (fn ResponderFunc) WriteResponse(rw http.ResponseWriter, pr runtime.Producer) {
	fn(rw, pr)
}
```


not implemented

```go
package middleware

import (
	"net/http"

	"github.com/go-openapi/runtime"
)

type errorResp struct {
	code     int
	response interface{}
	headers  http.Header
}

func (e *errorResp) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {
	for k, v := range e.headers {
		for _, val := range v {
			rw.Header().Add(k, val)
		}
	}
	if e.code > 0 {
		rw.WriteHeader(e.code)
	} else {
		rw.WriteHeader(http.StatusInternalServerError)
	}
	if err := producer.Produce(rw, e.response); err != nil {
		panic(err)
	}
}

// NotImplemented the error response when the response is not implemented
func NotImplemented(message string) Responder {
	return &errorResp{http.StatusNotImplemented, message, make(http.Header)}
}
```
