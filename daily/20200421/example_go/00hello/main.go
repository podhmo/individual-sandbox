package main

import (
	"fmt"

	"github.com/PuerkitoBio/goquery"
	"gopkg.in/headzoo/surf.v1"
)

func main() {
	// Create a new browser and open reddit.
	bow := surf.NewBrowser()
	err := bow.Open("https://reddit.com")
	if err != nil {
		panic(err)
	}

	// Outputs: "reddit: the front page of the internet"
	fmt.Println(bow.Title())

	// Click the link for the newest submissions.
	bow.Click("a.new")

	// Outputs: "newest submissions: reddit.com"
	fmt.Println(bow.Title())

	// Log in to the site.
	fm, _ := bow.Form("form.login-form")
	fm.Input("user", "JoeRedditor")
	fm.Input("passwd", "d234rlkasd")
	if fm.Submit() != nil {
		panic(err)
	}

	// Go back to the "newest submissions" page, bookmark it, and
	// print the title of every link on the page.
	bow.Back()
	bow.Bookmark("reddit-new")
	bow.Find("a.title").Each(func(_ int, s *goquery.Selection) {
		fmt.Println(s.Text())
	})
}
