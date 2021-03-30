package main

import (
	"log"
	"os"

	"github.com/hashicorp/hcl/v2/gohcl"
	"github.com/hashicorp/hcl/v2/hclwrite"
)

func main() {
	log.SetPrefix("  ")
	log.SetFlags(0)

	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

type Service struct {
	Name string   `hcl:"name,label"`
	Exe  []string `hcl:"executable"`
}
type Constraints struct {
	OS   string `hcl:"os"`
	Arch string `hcl:"arch"`
}
type App struct {
	Name        string       `hcl:"name"`
	Desc        string       `hcl:"description"`
	Constraints *Constraints `hcl:"constraints,block"`
	Services    []Service    `hcl:"service,block"`
}

func run() error {
	app := App{
		Name: "awesome-app",
		Desc: "Such an awesome application",
		Constraints: &Constraints{
			OS:   "linux",
			Arch: "amd64",
		},
		Services: []Service{
			{
				Name: "web",
				Exe:  []string{"./web", "--listen=:8080"},
			},
			{
				Name: "worker",
				Exe:  []string{"./worker"},
			},
		},
	}

	f := hclwrite.NewEmptyFile()
	gohcl.EncodeIntoBody(&app, f.Body())
	// fmt.Printf("%s", f.Bytes())

	if _, err := f.WriteTo(os.Stdout); err != nil {
		return err
	}
	return nil
}
