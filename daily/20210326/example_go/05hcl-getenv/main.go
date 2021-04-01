package main

import (
	"log"
	"os"

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

// or https://pkg.go.dev/github.com/hashicorp/hcl/v2/gohcl
//
// - attr (the default) indicates that the value is to be populated from an attribute
// - block indicates that the value is to populated from a block
// - label indicates that the value is to populated from a block label
// - optional is the same as attr, but the field is optional
// - remain indicates that the value is to be populated from the remaining body after populating other fields

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

var GetenvFunc = function.New(&function.Spec{
	Params: []function.Parameter{
		{
			Name:             "varname",
			Type:             cty.String,
			AllowDynamicType: true,
		},
		{
			Name:             "default",
			Type:             cty.String,
			AllowDynamicType: true,
		},
	},
	Type: function.StaticReturnType(cty.String),
	Impl: func(args []cty.Value, retType cty.Type) (cty.Value, error) {
		in := args[0].AsString()
		if v := os.Getenv(in); v != "" {
			return cty.StringVal(v), nil
		}
		return args[1], nil
	},
})

func init() {
	specFuncs["getenv"] = GetenvFunc
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
