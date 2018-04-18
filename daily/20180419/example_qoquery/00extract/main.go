package main

import (
	"fmt"
	"log"
	"os"
	"strings"

	"github.com/PuerkitoBio/goquery"
	kingpin "gopkg.in/alecthomas/kingpin.v2"
)

type opt struct {
	url string
}

func main() {
	var opt opt
	app := kingpin.New("extract-users", "extract users")
	app.Arg("url", "target url").StringVar(&opt.url)

	if _, err := app.Parse(os.Args[1:]); err != nil {
		app.FatalUsage(err.Error())
	}

	if err := run(opt.url); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run(url string) error {
	doc, err := goquery.NewDocument(url)
	if err != nil {
		return err
	}
	doc.Find(".social a").Each(func(n int, qs *goquery.Selection) {
		if href, ok := qs.Attr("href"); ok {
			if strings.HasPrefix(href, "https://twitter.com") {
				fmt.Println(strings.Split(href, "?screen_name=")[1])
			}
		}
	})
	return nil
}
