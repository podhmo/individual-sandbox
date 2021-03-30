package main

import (
	"fmt"
	"log"

	"github.com/hashicorp/hcl/v2"
	"github.com/hashicorp/hcl/v2/hclsimple"
	"github.com/zclconf/go-cty/cty"
	"github.com/zclconf/go-cty/cty/function"
	"github.com/zclconf/go-cty/cty/function/stdlib"
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

var specFuncs = map[string]function.Function{
	"abs":        stdlib.AbsoluteFunc,
	"coalesce":   stdlib.CoalesceFunc,
	"concat":     stdlib.ConcatFunc,
	"hasindex":   stdlib.HasIndexFunc,
	"int":        stdlib.IntFunc,
	"jsondecode": stdlib.JSONDecodeFunc,
	"jsonencode": stdlib.JSONEncodeFunc,
	"length":     stdlib.LengthFunc,
	"lower":      stdlib.LowerFunc,
	"max":        stdlib.MaxFunc,
	"min":        stdlib.MinFunc,
	"reverse":    stdlib.ReverseFunc,
	"strlen":     stdlib.StrlenFunc,
	"substr":     stdlib.SubstrFunc,
	"upper":      stdlib.UpperFunc,
}

var HelloFunc = function.New(&function.Spec{
	Params: []function.Parameter{
		{
			Name:             "str",
			Type:             cty.String,
			AllowDynamicType: true,
		},
	},
	Type: function.StaticReturnType(cty.String),
	Impl: func(args []cty.Value, retType cty.Type) (cty.Value, error) {
		in := args[0].AsString()
		out := fmt.Sprintf("Hello, %q", in)
		return cty.StringVal(out), nil
	},
})

func init() {
	specFuncs["hello"] = HelloFunc
}

func run() error {
	var config Config
	filename := "config.hcl"
	log.Printf("Load file %q", filename)

	hclctx := &hcl.EvalContext{
		Variables: map[string]cty.Value{},
		Functions: specFuncs,
	}
	err := hclsimple.DecodeFile(filename, hclctx, &config)
	if err != nil {
		return err
	}
	log.Printf("Configuration is %#v", config)
	return nil
}
