package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"strings"

	"github.com/hashicorp/hcl/v2"
	"github.com/hashicorp/hcl/v2/hcldec"
	"github.com/hashicorp/hcl/v2/hclparse"
	"github.com/zclconf/go-cty/cty"
	"github.com/zclconf/go-cty/cty/function"
	ctyjson "github.com/zclconf/go-cty/cty/json"
	"golang.org/x/crypto/ssh/terminal"
)

func main() {
	args := os.Args
	if err := run(args); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

var parser = hclparse.NewParser()

func run(args []string) error {
	var diags hcl.Diagnostics
	var bodies []hcl.Body

	if len(args) == 0 {
		src, err := ioutil.ReadAll(os.Stdin)
		if err != nil {
			return fmt.Errorf("failed to read stdin: %s", err)
		}

		f, fDiags := parser.ParseHCL(src, "<stdin>")
		diags = append(diags, fDiags...)
		if !fDiags.HasErrors() {
			bodies = append(bodies, f.Body)
		}
	} else {
		for _, filename := range args {
			var f *hcl.File
			var fDiags hcl.Diagnostics
			if strings.HasSuffix(filename, ".json") {
				f, fDiags = parser.ParseJSONFile(filename)
			} else {
				f, fDiags = parser.ParseHCLFile(filename)
			}
			diags = append(diags, fDiags...)
			if !fDiags.HasErrors() {
				bodies = append(bodies, f.Body)
			}
		}
	}

	color := terminal.IsTerminal(int(os.Stderr.Fd()))
	w, _, err := terminal.GetSize(int(os.Stdout.Fd()))
	if err != nil {
		w = 80
	}
	diagWr := hcl.NewDiagnosticTextWriter(os.Stderr, parser.Files(), uint(w), color)

	if diags.HasErrors() {
		diagWr.WriteDiagnostics(diags)
		flush(diagWr)
		os.Exit(2)
	}

	var body hcl.Body
	switch len(bodies) {
	case 0:
		// should never happen, but... okay?
		body = hcl.EmptyBody()
	case 1:
		body = bodies[0]
	default:
		body = hcl.MergeBodies(bodies)
	}

	ctx := &hcl.EvalContext{
		Variables: map[string]cty.Value{},
		Functions: map[string]function.Function{},
	}
	var spec hcldec.Spec
	val, decDiags := hcldec.Decode(body, spec, ctx)
	diags = append(diags, decDiags...)

	if diags.HasErrors() {
		diagWr.WriteDiagnostics(diags)
		flush(diagWr)
		os.Exit(2)
	}

	wantType := val.Type()
	// We'll instead ask to encode as dynamic, which will make the
	// marshaler include type information.
	wantType = cty.DynamicPseudoType

	out, err := ctyjson.Marshal(val, wantType)
	if err != nil {
		return err
	}

	// // hcldec will include explicit nulls where an ObjectSpec has a spec
	// // that refers to a missing item, but that'll probably be annoying for
	// // a consumer of our output to deal with so we'll just strip those
	// // out and reduce to only the non-null values.
	// if !*keepNulls {
	// 	out = stripJSONNullProperties(out)
	// }

	target := os.Stdout
	fmt.Fprintf(target, "%s\n", out)
	return nil
}

type flusher interface {
	Flush() error
}

func flush(maybeFlusher interface{}) error {
	if f, ok := maybeFlusher.(flusher); ok {
		return f.Flush()
	}
	return nil
}
