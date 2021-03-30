package main

import (
	"log"

	"github.com/hashicorp/hcl/v2"
	"github.com/hashicorp/hcl/v2/hclsimple"
	"github.com/zclconf/go-cty/cty"
	"github.com/zclconf/go-cty/cty/function"
)

func main() {
	log.SetPrefix("  ")
	log.SetFlags(0)

	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type Config struct {
	Variable []Variable `hcl:"variable,block"`
}

type Variable struct {
	Value   string `hcl:"name,label"`
	Default string `hcl:"default"`
}

func run() error {
	var config Config
	filename := "config.hcl"
	log.Printf("Load file %q", filename)

	hclctx := &hcl.EvalContext{
		Variables: map[string]cty.Value{},
		Functions: map[string]function.Function{},
	}
	err := hclsimple.DecodeFile(filename, hclctx, &config)
	if err != nil {
		return err
	}
	log.Printf("Configuration is %#v", config)
	return nil
}
