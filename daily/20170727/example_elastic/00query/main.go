package main

import (
	"log"

	"github.com/k0kubun/pp"
	"github.com/olivere/elastic"
)

// Q :
func Q(words []string) (elastic.Query, error) {
	q := elastic.NewBoolQuery()
	for _, word := range words {
		q.Must(elastic.NewMultiMatchQuery(word,
			"name^3", "page.title^5", "page.description^2", "gaKeywords",
		))
	}
	return q, nil
}

func main() {
	words := []string{"foo", "bar", "boo"}
	q, err := Q(words)
	if err != nil {
		log.Fatal(err)
	}
	pp.Println(q)
}
