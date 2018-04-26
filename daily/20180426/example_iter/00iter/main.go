package main

import (
	"encoding/json"
	"log"
	"reflect"

	"github.com/k0kubun/pp"
)

// SearchResult is the result of a search in Elasticsearch.
type SearchResult struct {
	Hits *SearchHits `json:"hits"` // the actual search hits
}

// SearchHits specifies the list of search hits.
type SearchHits struct {
	TotalHits int64        `json:"total"` // total number of hits found
	Hits      []*SearchHit `json:"hits"`  // the actual hits returned
}

// SearchHit is a single hit.
type SearchHit struct {
	Source *json.RawMessage `json:"_source"` // stored document source
}

// Each is a utility function to iterate over all hits. It saves you from
// checking for nil values. Notice that Each will ignore errors in
// serializing JSON and hits with empty/nil _source will get an empty
// value
func (r *SearchResult) Each(typ reflect.Type) []interface{} {
	if r.Hits == nil || r.Hits.Hits == nil || len(r.Hits.Hits) == 0 {
		return nil
	}
	var slice []interface{}
	for _, hit := range r.Hits.Hits {
		v := reflect.New(typ).Elem()
		if hit.Source == nil {
			slice = append(slice, v.Interface())
			continue
		}
		if err := json.Unmarshal(*hit.Source, v.Addr().Interface()); err == nil {
			slice = append(slice, v.Interface())
		}
	}
	return slice
}

func main() {
	response := `
{
  "hits": {
    "hits": [
      {"_source": {"id": 1, "name": "foo", "age": 20}},
      {"_source": {"id": 2, "name": "bar", "age": 21}},
      {"_source": {"id": 3, "name": "boo", "age": 20}}
    ],
    "total": 3
  }
}
`
	var result SearchResult
	if err := json.Unmarshal([]byte(response), &result); err != nil {
		log.Fatal(err)
	}

	for _, row := range result.Each(reflect.TypeOf(person{})) {
		row, ok := row.(person)
		pp.Println(row, ok)
	}
}

type person struct {
	ID   int
	Name string
	Age  int
}
