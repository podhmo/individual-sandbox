package main

import (
	"fmt"
	"log"

	"github.com/hashicorp/hcl/v2"
	"github.com/hashicorp/hcl/v2/hclsyntax"
	"github.com/hashicorp/terraform/addrs"
	"github.com/hashicorp/terraform/configs/configschema"
	"github.com/hashicorp/terraform/instances"
	"github.com/hashicorp/terraform/lang"
	"github.com/hashicorp/terraform/tfdiags"
	"github.com/zclconf/go-cty/cty"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("! %+v", err)
	}
}

type dataForTests struct {
	CountAttrs     map[string]cty.Value
	ForEachAttrs   map[string]cty.Value
	Resources      map[string]cty.Value
	LocalValues    map[string]cty.Value
	Modules        map[string]cty.Value
	PathAttrs      map[string]cty.Value
	TerraformAttrs map[string]cty.Value
	InputVariables map[string]cty.Value
}

var _ lang.Data = &dataForTests{}

func (d *dataForTests) StaticValidateReferences(refs []*addrs.Reference, self addrs.Referenceable) tfdiags.Diagnostics {
	return nil // does nothing in this stub implementation
}

func (d *dataForTests) GetCountAttr(addr addrs.CountAttr, rng tfdiags.SourceRange) (cty.Value, tfdiags.Diagnostics) {
	return d.CountAttrs[addr.Name], nil
}

func (d *dataForTests) GetForEachAttr(addr addrs.ForEachAttr, rng tfdiags.SourceRange) (cty.Value, tfdiags.Diagnostics) {
	return d.ForEachAttrs[addr.Name], nil
}

func (d *dataForTests) GetResource(addr addrs.Resource, rng tfdiags.SourceRange) (cty.Value, tfdiags.Diagnostics) {
	return d.Resources[addr.String()], nil
}

func (d *dataForTests) GetInputVariable(addr addrs.InputVariable, rng tfdiags.SourceRange) (cty.Value, tfdiags.Diagnostics) {
	return d.InputVariables[addr.Name], nil
}

func (d *dataForTests) GetLocalValue(addr addrs.LocalValue, rng tfdiags.SourceRange) (cty.Value, tfdiags.Diagnostics) {
	return d.LocalValues[addr.Name], nil
}

func (d *dataForTests) GetModule(addr addrs.ModuleCall, rng tfdiags.SourceRange) (cty.Value, tfdiags.Diagnostics) {
	return d.Modules[addr.String()], nil
}

func (d *dataForTests) GetModuleInstanceOutput(addr addrs.AbsModuleCallOutput, rng tfdiags.SourceRange) (cty.Value, tfdiags.Diagnostics) {
	// This will panic if the module object does not have the requested attribute
	obj := d.Modules[addr.Call.String()]
	return obj.GetAttr(addr.Name), nil
}

func (d *dataForTests) GetPathAttr(addr addrs.PathAttr, rng tfdiags.SourceRange) (cty.Value, tfdiags.Diagnostics) {
	return d.PathAttrs[addr.Name], nil
}

func (d *dataForTests) GetTerraformAttr(addr addrs.TerraformAttr, rng tfdiags.SourceRange) (cty.Value, tfdiags.Diagnostics) {
	return d.TerraformAttrs[addr.Name], nil
}

func run() error {
	nestedObjTy := cty.Object(map[string]cty.Type{
		"boop": cty.String,
	})
	schema := &configschema.Block{
		Attributes: map[string]*configschema.Attribute{
			"foo":         {Type: cty.String, Optional: true},
			"list_of_obj": {Type: cty.List(nestedObjTy), Optional: true},
		},
		BlockTypes: map[string]*configschema.NestedBlock{
			"bar": {
				Nesting: configschema.NestingMap,
				Block: configschema.Block{
					Attributes: map[string]*configschema.Attribute{
						"baz": {Type: cty.String, Optional: true},
					},
				},
			},
		},
	}
	data := &dataForTests{
		LocalValues: map[string]cty.Value{
			"greeting": cty.StringVal("howdy"),
			"list": cty.ListVal([]cty.Value{
				cty.StringVal("elem0"),
				cty.StringVal("elem1"),
			}),
			"map": cty.MapVal(map[string]cty.Value{
				"key1": cty.StringVal("val1"),
				"key2": cty.StringVal("val2"),
			}),
		},
	}

	test := struct {
		Config  string
		Want    map[string]cty.Value
	}{
		Config:`
			foo = "whoop"
			bar "static0" {
				baz = "s0"
			}
			dynamic "bar" {
				for_each = local.list
				labels = [bar.value]
				content {
					baz = bar.key
				}
			}
			bar "static1" {
				baz = "s1"
			}
			dynamic "bar" {
				for_each = local.map
				labels = [bar.key]
				content {
					baz = bar.value
				}
			}
			bar "static2" {
				baz = "s2"
			}
			`,
		Want: cty.ObjectVal(map[string]cty.Value{
				"foo":         cty.StringVal("whoop"),
				"list_of_obj": cty.NullVal(cty.List(nestedObjTy)),
				"bar": cty.MapVal(map[string]cty.Value{
					"key1": cty.ObjectVal(map[string]cty.Value{
						"baz": cty.StringVal("val1"),
					}),
					"key2": cty.ObjectVal(map[string]cty.Value{
						"baz": cty.StringVal("val2"),
					}),
					"elem0": cty.ObjectVal(map[string]cty.Value{
						"baz": cty.StringVal("0"),
					}),
					"elem1": cty.ObjectVal(map[string]cty.Value{
						"baz": cty.StringVal("1"),
					}),
					"static0": cty.ObjectVal(map[string]cty.Value{
						"baz": cty.StringVal("s0"),
					}),
					"static1": cty.ObjectVal(map[string]cty.Value{
						"baz": cty.StringVal("s1"),
					}),
					"static2": cty.ObjectVal(map[string]cty.Value{
						"baz": cty.StringVal("s2"),
					}),
				}),
			}),
		}

	file, parseDiags := hclsyntax.ParseConfig([]byte(test.Config), "", hcl.Pos{Line: 1, Column: 1})
	if len(parseDiags) != 0 {
		log.Print("unexpected diagnostics during parse")
		for _, diag := range parseDiags {
			log.Printf("- %s", diag)
		}
		return fmt.Errorf("unexpected diagnostics during parse")
	}

	body := file.Body
	scope := &lang.Scope{
		Data: data,
	}

	gotVal, ctxDiags := scope.Eval(body, test.Self, schema, test.KeyData)
	if ctxDiags.HasErrors() {
		t.Fatal(ctxDiags.Err())
	}

	wantVal := cty.ObjectVal(test.Want)

	if !gotVal.RawEquals(wantVal) {
		t.Errorf(
			"wrong result\nexpr: %s\ngot:  %#v\nwant: %#v",
			test.Config, gotVal, wantVal,
		)
	}
}
