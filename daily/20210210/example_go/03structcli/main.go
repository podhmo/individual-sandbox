package main

import (
	"fmt"
	"log"
	"os"
)

type Params struct {
	// Name 名前
	Name string `arg:"name"`

	// ShowFullName フルネームで表示するオプション
	ShowFullName bool `arg:"show-full-name"`

	Args []string // rest args
}

func main() {
	var params Params
	app := structcli.NewApp("app")
	if err := app.Parse(
		&params,
		structcli.WithRestArgs("Args"),
		structcli.WithEnvVars(),
	); err != nil {
		app.Usage(err)
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
