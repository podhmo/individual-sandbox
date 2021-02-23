//go:generate go run main_gen.go

package main

import (
	"fmt"
	"log"
	"os"
	"strings"

	"github.com/spf13/pflag"
)

// START: GENERATE FROM structgen
type Params struct {
	// Name 名前
	Name string `arg:"name"`

	// ShowFullName フルネームで表示するオプション
	ShowFullName bool `arg:"show-full-name"`

	Args []string // rest args
}

func main() {
	name := "app"
	fs := pflag.NewFlagSet(name, pflag.ExitOnError)

	var params Params
	fs.StringVar(&params.Name, "name", "", "名前")
	fs.BoolVar(&params.ShowFullName, "show-full-name", false, "フルネームで表示するオプション")

	args := os.Args[1:]
	if err := fs.Parse(args); err != nil {
		fs.Usage()
		if err == pflag.ErrHelp {
			os.Exit(0)
		}
		os.Exit(2)
	}

	params.Args = fs.Args()

	// WithEnvVars
	fs.VisitAll(func(f *pflag.Flag) {
		envname := strings.ReplaceAll(strings.ToUpper(f.Name), "-", "_")
		log.Printf("lookup %s", envname)
		if v := os.Getenv(envname); v != "" {
			if err := fs.Set(f.Name, v); err != nil {
				fmt.Fprintf(os.Stderr, "parse error, on envvar %s=%v, %v", envname, v, err)
				os.Exit(2)
			}
		}
	})

	// required
	if params.Name == "" {
		fmt.Fprintln(os.Stderr, "name is required")
		os.Exit(2)
	}

	if err := Run(params); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

// END: GENERATE FROM structgen*/
func Run(params Params) error {
	if params.ShowFullName {
		fmt.Printf("full main: %[1]T: %+#[1]v\n", params)
	} else {
		fmt.Printf("main: %[1]T: %+#[1]v\n", params)
	}
	return nil
}
