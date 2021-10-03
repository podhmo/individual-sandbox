package main

import (
	"fmt"
	"log"

	"github.com/go-playground/validator/v10"
	"github.com/k0kubun/pp"
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

var validate = validator.New()

func Validate(ob interface{}) error {
	// errorをマージしたり苦労は色々ある
	if err := validate.Struct(ob); err != nil {
		return err
	}
	if v, ok := ob.(interface{ Validate() error }); ok {
		return v.Validate()
	}
	return nil
}

type PostArticleCommentInput struct { // 名前が長くなるのが好きではないんだけど良い方法があまりない
	Text string `validate:"required"`
}

func PostArticleComment(input PostArticleCommentInput, articleID int64) (*Article, error) {
	if err := Validate(input); err != nil {
		return nil, err
	}
	article, ok := articles[articleID]
	if !ok {
		return nil, fmt.Errorf("not found")
	}
	article.Comments = append(article.Comments, &Comment{
		Author: "someone",
		Text:   input.Text,
	})
	return article, nil
}

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err) // 個人的にはlog.Fatal(run())とかは嫌い。どこでキャッチされたかわからないので。
	}
}

func run() error {
	pp.Println(articles)
	fmt.Println("----------------------------------------")
	var articleID int64 = 1
	result, err := PostArticleComment(PostArticleCommentInput{Text: "hello"}, articleID)
	if err != nil {
		return err
	}
	fmt.Println("got", result)
	fmt.Println("----------------------------------------")
	pp.Println(articles)
	return nil
}
