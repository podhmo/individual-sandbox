// +build !mock

package main

import (
	"fmt"
	"m/02tag/news"
)

type News = news.News

func main() {
	news := &News{}
	fmt.Println(news.Get())
}
