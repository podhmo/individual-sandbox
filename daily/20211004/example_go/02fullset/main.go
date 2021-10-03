package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"strconv"
	"time"

	"github.com/go-chi/chi/v5"
	"github.com/go-chi/chi/v5/middleware"
	"github.com/go-playground/validator/v10"
	"github.com/gorilla/schema"
)

type Article struct {
	ID       int64      `json:"id"`
	Title    string     `json:"title"`
	Text     string     `json"text"`
	Comments []*Comment `json:"comments"`
}
type Comment struct {
	Author string `json:"author"`
	Text   string `json"text"`
}

var articles = map[int64]*Article{
	1: &Article{
		ID:    1,
		Title: "foo",
	},
}
var decoder = schema.NewDecoder() // 場合によってはsync.Pool
var validate = validator.New()

// net/http.HandlerFuncをそのまま使うのはだるいので func(...) error のような関数を渡せるようにラップすることが多い
// 雑に作るならstatusコードは404,400,200,500程度で良いのでは説
func PostArticleComment(w http.ResponseWriter, req *http.Request) {
	var pathItem struct {
		ID int64 `path:"articleId,required"`
	}

	// 値のbindingまで含めて、統一的に扱うならflatなmapはgorilla/schemaを使うのが楽かも（？）
	// 別解としてstrconv.ParseInt()的なものやそのヘルパー的なもので個別に頑張るなどがある。
	decoder.SetAliasTag("path")
	{
		params := map[string][]string{
			"articleId": {chi.URLParam(req, "articleId")},
		}
		if err := decoder.Decode(&pathItem, params); err != nil {
			w.WriteHeader(http.StatusNotFound)
			// クライアントに露出するようなAPIでは、"schema: error converting value for \"articleId\"" みたいなメッセージを出力させないかも。
			fmt.Fprintf(w, `{"message": %q}`, err.Error())
			return
		}
		// 必要ならここでregexのチェック（通常はroutingの部分でやっていることが多いので基本的には値のbindingだけで良いと思っている）
	}

	article, ok := articles[pathItem.ID]
	if !ok {
		// DBアクセスして存在しなかったら404にしているが、コレクションを返す系のresourceなら空配列を200で返す
		// (/articles/<article_id>/comments とかの場合、404はarticleがない、200で空配列はcommentされていないarticleが存在)
		w.WriteHeader(http.StatusNotFound)
		fmt.Fprintf(w, `{"message": %q}`, "not found")
		return
	}

	// query string あれば

	var data struct {
		Text string `json:"text" validate:"required"`
	}
	{
		decoder := json.NewDecoder(req.Body)
		if err := decoder.Decode(&data); err != nil {
			w.WriteHeader(http.StatusBadRequest)
			fmt.Fprintf(w, `{"message": %q}`, err.Error())
			return
		}
		defer req.Body.Close()
		if err := validate.Struct(&data); err != nil {
			w.WriteHeader(http.StatusBadRequest)
			fmt.Fprintf(w, `{"message": %q}`, err.Error())
			return
		}
	}

	article.Comments = append(article.Comments, &Comment{
		Author: "someone",
		Text:   data.Text,
	})

	{
		buf := new(bytes.Buffer)
		enc := json.NewEncoder(buf)
		enc.SetIndent("", "  ")                     // 普通はoptionでハンドリングする。 JSON(w, req, <data>, 200)とか渡すか c.JSON(<data>, 200) のときのcに含める
		if err := enc.Encode(article); err != nil { // 全体を返すかは考える余地がある。JSONへの変換に失敗したときのことを考えると一度bytes.Bufferなどに出力したほうが良いかも。ただほとんど壊れないだとかサイズが大きいだとかの場合は要検討。
			w.WriteHeader(http.StatusInternalServerError)
			fmt.Fprintf(w, `{"message": %q}`, err.Error())
			return
		}
		if _, err := io.Copy(w, buf); err != nil {
			w.WriteHeader(500) // おそらくこれは効かない処理なのだけれど（初回write時にstatusコードが設定されていなかったら200を返す）
			fmt.Fprintf(w, `{"message": "%q"}`, err.Error())
			// panicリカバリーだったりエラーログをいい感じに扱うmiddlewareは入れておく
		}
	}
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err) // 個人的にはlog.Fatal(run())とかは嫌い。どこでキャッチされたかわからないので。
	}
}

func run() error {
	r := chi.NewRouter()
	// 本当はこの辺でmiddlewareを真面目に設定する
	// A good base middleware stack
	r.Use(middleware.RequestID)
	r.Use(middleware.RealIP)
	r.Use(middleware.Logger)
	r.Use(middleware.Recoverer)
	r.Use(middleware.Timeout(60 * time.Second))

	// routingも普通は関数に切り出す
	r.Post("/articles/{articleId}/comments", PostArticleComment)
	// 動いているかどうかわからないのでhealth check的なendpointも用意しておく

	// 普通はListenAndServeをそのまま使わないかも。graceful stopとか諸々対応する
	// 設定ももう少し真面目に扱う
	port := 8888
	if v, err := strconv.Atoi(os.Getenv("PORT")); err == nil {
		port = v
	}
	addr := fmt.Sprintf(":%d", port)
	log.Println("listen ...", addr)
	return http.ListenAndServe(addr, r)
}
