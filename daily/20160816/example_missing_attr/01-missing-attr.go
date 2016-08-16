package main

// 不足した属性はゼロ値で初期化される

import (
	"fmt"
	"time"
)

// Data represents a xxxx's data.
type Data struct {
	ID        *string    `json:"id,omitempty"`
	CreatedAt *time.Time `json:"created_at,omitempty"`
}

func main() {
	data := Data{}
	fmt.Printf("data: %[1]T -- %#[1]v\n", data)
    // data: main.Data main.Data{ID:(*string)(nil), CreatedAt:(*time.Time)(nil)}
}
