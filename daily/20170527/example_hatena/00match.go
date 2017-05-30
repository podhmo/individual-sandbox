package main

import (
	"errors"
	"regexp"
	"strings"

	"github.com/k0kubun/pp"
)

// Article :
type Article struct {
	Title      string
	Body       string
	Categories []string
}

var rx = regexp.MustCompile("# *((?:\\[[^\\]]+\\])*)? *(.+)")

var topLevelSectionRE = regexp.MustCompile("#[^#]")
var parseRE = regexp.MustCompile("# *((?:\\[[^\\]]+\\])*)? *(.+)")

func parseArticle(body string) (Article, error) {
	lines := strings.Split(body, "\n")
	for _, line := range lines {
		if topLevelSectionRE.MatchString(line) {
			separeted := parseRE.FindStringSubmatch(line)
			categoryArea := separeted[1]
			title := separeted[2]
			categories := parseCategories(categoryArea)
			return Article{Title: title, Categories: categories, Body: body}, nil
		}
	}
	return Article{}, errors.New("title is not found")
}

func parseCategories(s string) []string {
	if !strings.Contains(s, "]") {
		return []string{}
	}
	nodes := strings.Split(s, "]")
	categories := make([]string, 0, len(nodes))
	for _, node := range nodes {
		category := strings.TrimSpace(strings.TrimLeft(strings.TrimSpace(node), "["))
		if category == "" {
			continue
		}
		categories = append(categories, category)
	}
	return categories
}

func main() {
	{
		line := "# yamlの何か"
		pp.Println(parseArticle(line))
	}
	{
		line := "# [python]yamlの何か"
		pp.Println(parseArticle(line))
	}
	{
		line := "# [python][yaml]yamlの何か"
		pp.Println(parseArticle(line))
	}
}
