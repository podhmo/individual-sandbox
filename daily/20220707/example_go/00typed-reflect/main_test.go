package main

import (
	"context"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"reflect"
	"testing"
)

// http.Handlerは何を返すか言及できない
func Empty(w http.ResponseWriter, req *http.Request) {

}

// handlerを返す関数にすると初期化時に何らかの準備の処理を行うことができる

type FooInput struct{}
type FooOutput struct{}

func MakeHandler() http.HandlerFunc {
	var i FooInput
	var o FooOutput
	log.Printf("input is %T", i)
	log.Printf("output is %T", o)
	return func(w http.ResponseWriter, req *http.Request) {

	}
}

// ある値を返す関数にしてあげると、それを利用する処理として扱える
// しかし、特定のsignatureの関数に対応したそれであって、結局個別に設定しなければいけない。

func MakeHandler2(fn func(context.Context, FooInput) (*FooOutput, error)) http.HandlerFunc {
	log.Printf("fn is %v", reflect.TypeOf(fn))
	return func(w http.ResponseWriter, req *http.Request) {

	}
}

// genericsを使うことで、汎用的なhandler factoryを定義できるようになる。
// middlewareとの違いは、inputやoutputの型を認識できる点。
// しかし、他の依存やパラメーターを取ってくる方法が見えない(例えば query stringやheader)。
// また、Inputに依存しない関数はどうか？
// 簡単なCRUDのようなものならhandlerとしてliftできるかもしれない。

func MakeHandlerWithGenerics[Input any, Output any](fn func(context.Context, Input) (*Output, error)) http.HandlerFunc {
	log.Printf("fn is %v", reflect.TypeOf(fn))
	return func(w http.ResponseWriter, req *http.Request) {

	}
}

// また特殊化をsubset的なinterfaceにすることはできる
// https://gist.github.com/podhmo/7deb69f0c64a247867c56cf8ed48cc35

// 形状の変形ではなく、annotationのような機構として使ってみるのはどうか？
// Action的なものが存在したときに、その関数が func(ctx, I) (O, error) に限定されてしまう
// 型合わせのためだけのwrapはつら過ぎる。

// handlerの形状を限定してみる
// - GET, DELETE, HEAD :: func(context.Context) (Output, error)
// - POST, PUT, PATCH :: func(context.Context, Input) (Output, error)
// ここで、Inputは必ずrequest.Bodyからapplication/jsonとして扱って受け取る
// 不足している情報は？
// path item, query string, header

type DB struct{}

type Command[I any, O any] func(context.Context, I) (O, error)
type Query[O any] func(context.Context) (O, error)

type FooUsecase struct {
	DB *DB
}

func (u *FooUsecase) Revive(context.Context, FooInput) (*FooOutput, error) {
	return &FooOutput{}, nil
}

func MakeCommandHandlerWithGenerics[I any, O any](fn Command[I, O]) http.HandlerFunc {
	log.Printf("fn is %v", reflect.TypeOf(fn))
	return func(w http.ResponseWriter, req *http.Request) {
		var input I
		if err := json.NewDecoder(req.Body).Decode(&input); err != nil {
			w.WriteHeader(500)
			log.Printf("unexpected error (decode) %+v", err)
			fmt.Fprintln(w, `{"error": "internal server error"}`)
			return
		}
		output, err := fn(req.Context(), input)
		if err != nil {
			w.WriteHeader(500)
			log.Printf("unexpected error %+v", err)
			fmt.Fprintln(w, `{"error": "internal server error"}`)
			return
		}
		// need []byte ?
		if err := json.NewEncoder(w).Encode(output); err != nil {
			log.Printf("unexpected error (encode) %+v", err)
			fmt.Fprintln(w, `{"error": "internal server error"}`)
		}
	}
}

// query stringやheaderは？　となると結局、requestやresponseを受け取らなくてはいけないのでは？
// Inputがすべてを内包していればよいのでは？
func FooCommand(ctx context.Context, input struct {
	FooID string `json:"fooId" openapi:"path"` // `/foo/{fooId}``

	FooInput `openapi:"json"` // request.jsonBody

	Sort   string `json:"sort" enum:"asc,desc" openapi:"query"` // ?sort=desc
	Pretty bool   `json:"pretty" openapi:"query"`               // ?pretty=true

	XAuthKey string `json:"-" openapi:"header" openapi-name:"X-AUTH-KEY"` // X-AUTH-KEY: xxxxx
}) (output struct {
	FooID string `json:"fooId"`
}, err error) {
	output.FooID = input.FooID
	return
}

// ここにsummaryをかく
//
// ここに長々としたdescriptionを書く（問題はこれに実行時にはアクセスできないこと）
func MakeCommandHandlerWithGenerics2[I any, O any](fn Command[I, O]) http.HandlerFunc {
	log.Printf("fn is %v", reflect.TypeOf(fn))
	// ここでFooCommandのinputを取り出して、reflectでチェックできる
	// query string, headers
	// jsonは埋め込み

	// この辺は計算
	// queryKeys := []string{"pretty:Pretty", "sort:Sort"}
	// headerKeys := []string{"X-AUTH-KEY:XAuthKey"}
	// hasData := true

	// 手軽に済ませたいならreflectでbind?

	// これで登録できると良いのでは？
	handler := func(w http.ResponseWriter, req *http.Request) {
		var input I
		if err := json.NewDecoder(req.Body).Decode(&input); err != nil {
			w.WriteHeader(500)
			log.Printf("unexpected error (decode) %+v", err)
			fmt.Fprintln(w, `{"error": "internal server error"}`)
			return
		}
		output, err := fn(req.Context(), input)
		if err != nil {
			w.WriteHeader(500)
			log.Printf("unexpected error %+v", err)
			fmt.Fprintln(w, `{"error": "internal server error"}`)
			return
		}
		// need []byte ?
		if err := json.NewEncoder(w).Encode(output); err != nil {
			log.Printf("unexpected error (encode) %+v", err)
			fmt.Fprintln(w, `{"error": "internal server error"}`)
		}
	}
	// registry[handler] = <metadata>
	return handler
}

func TestIT(t *testing.T) {
	fn := func(context.Context, FooInput) (*FooOutput, error) { return nil, fmt.Errorf("hmm") }
	u := &FooUsecase{DB: &DB{}}

	mux := http.NewServeMux()
	mux.HandleFunc("/0", Empty)
	mux.HandleFunc("/1", MakeHandler())
	mux.HandleFunc("/2", MakeHandler2(fn))

	// ここでu.Reviveのsignatureを使ってあれこれできる (e.g. openAPIのdocに登録)
	mux.HandleFunc("/3", MakeCommandHandlerWithGenerics(u.Revive))
}
