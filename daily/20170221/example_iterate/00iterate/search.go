package main

import (
	"encoding/json"
	"reflect"
)

// SearchResult :
type SearchResult struct {
	Hits *SearchHits `json:"hits"`
}

// Each :
func (r *SearchResult) Each(typ reflect.Type) []interface{} {
	if r.Hits == nil || r.Hits.Hits == nil || len(r.Hits.Hits) == 0 {
		return nil
	}
	var slice []interface{}
	for _, hit := range r.Hits.Hits {
		v := reflect.New(typ).Elem()
		if err := json.Unmarshal(*hit.Source, v.Addr().Interface()); err == nil {
			slice = append(slice, v.Interface())
		}
	}
	return slice
}

// SearchHits :
type SearchHits struct {
	TotalHits int64        `json:"totalHits"`
	Hits      []*SearchHit `json:"hits"`
}

// SearchHit :
type SearchHit struct {
	Source *json.RawMessage `json:"_source"`
}
