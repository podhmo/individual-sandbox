## hcl

そういえば、使って見てない。

- https://github.com/hashicorp/hcl
- https://github.com/hashicorp/hcl/blob/master/test-fixtures/assign_deep.hcl

### terraform

terraform側にevalの情報があるのか。

terraform/lang/scope.go

- scope.EvalContext(refs)
- scope.ExpandBlock(body, schema)
- scope.EvalBlock(body, schema)
- scope.EvalSelfBlock(body, test.Self, schema, test.KeyData)

```console
$ go doc github.com/hashicorp/terraform/lang.Scope
package lang // import "github.com/hashicorp/terraform/lang"

type Scope struct {
        // Data is used to resolve references in expressions.
        Data Data

        // SelfAddr is the address that the "self" object should be an alias of,
        // or nil if the "self" object should not be available at all.
        SelfAddr addrs.Referenceable

        // BaseDir is the base directory used by any interpolation functions that
        // accept filesystem paths as arguments.
        BaseDir string

        // PureOnly can be set to true to request that any non-pure functions
        // produce unknown value results rather than actually executing. This is
        // important during a plan phase to avoid generating results that could
        // then differ during apply.
        PureOnly bool

        // Has unexported fields.
}
    Scope is the main type in this package, allowing dynamic evaluation of
    blocks and expressions based on some contextual information that informs
    which variables and functions will be available.

func (s *Scope) EvalBlock(body hcl.Body, schema *configschema.Block) (cty.Value, tfdiags.Diagnostics)
func (s *Scope) EvalContext(refs []*addrs.Reference) (*hcl.EvalContext, tfdiags.Diagnostics)
func (s *Scope) EvalExpr(expr hcl.Expression, wantType cty.Type) (cty.Value, tfdiags.Diagnostics)
func (s *Scope) EvalReference(ref *addrs.Reference, wantType cty.Type) (cty.Value, tfdiags.Diagnostics)
func (s *Scope) EvalSelfBlock(body hcl.Body, self cty.Value, schema *configschema.Block, ...) (cty.Value, tfdiags.Diagnostics)
func (s *Scope) ExpandBlock(body hcl.Body, schema *configschema.Block) (hcl.Body, tfdiags.Diagnostics)
func (s *Scope) Functions() map[string]function.Function
func (s *Scope) SetActiveExperiments(active experiments.Set)
```

## openAPI $refにdescriptionを付ける方法

- allOf

### swagger-marshmallow-codegen

動くようになっていないかも？allOf

## go build conditionで範囲外になるっけ？

そして、go runで実行可能であれば良い。
