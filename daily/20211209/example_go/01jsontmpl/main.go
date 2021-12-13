package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
)

type builder struct {
}

func (b *builder) ToReader(params interface{}) (*bytes.Buffer, error) {
	var buf bytes.Buffer
	if err := json.NewEncoder(&buf).Encode(params); err != nil {
		return nil, err
	}
	return &buf, nil
}

type PersonBuilder struct {
	builder *builder
	tmpl    string
	params  struct {
		Name string
		Age  int
	}
}

func (b *PersonBuilder) ToReader() (*bytes.Buffer, error) {
	replacer := strings.NewReplacer("$Name$", b.params.Name, "$Age$", strconv.Itoa(b.params.Age))
	s := replacer.Replace(b.tmpl)
	return bytes.NewBufferString(s), nil
}

func (b *PersonBuilder) Name(name string) *PersonBuilder {
	copied := b.params
	copied.Name = name
	return &PersonBuilder{builder: b.builder, tmpl: b.tmpl,
		params: copied,
	}
}

func NewPerson() *PersonBuilder {
	tmpl := `{"name": "$Name$", "age": 20}` // 本当はもう少し真面目にinjection的なものの対策をする
	return &PersonBuilder{
		builder: &builder{},
		tmpl:    tmpl,
	}
}

func WithExtras(toReader func() (*bytes.Buffer, error)) (*bytes.Buffer, error) {
	base, err := toReader()
	if err != nil {
		return nil, fmt.Errorf("inner: %w", err)
	}
	var v struct {
		Limit int `json:"limit"`
	}
	v.Limit = 10
	tail, err := json.Marshal(v)
	if err != nil {
		return nil, err
	}

	head := base.Bytes()
	return bytes.NewBuffer(append(head[:len(head)-1], append([]byte{',', ' '}, tail[1:]...)...)), nil
}

func main() {
	x, _ := NewPerson().Name("foo").ToReader()
	io.Copy(os.Stdout, x)
	fmt.Println("\n----------------------------------------")
	y, _ := WithExtras(NewPerson().Name("foo").ToReader)
	io.Copy(os.Stdout, y)
}
