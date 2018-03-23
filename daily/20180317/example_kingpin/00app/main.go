package main

import (
	"log"
	"os"

	"github.com/pkg/errors"
	"gopkg.in/alecthomas/kingpin.v2"
)

type opt struct {
	path   string
	dryrun bool
}

func main() {
	var opt opt
	app := kingpin.New("app", "my app")
	app.Flag("dryrun", "dry run option").BoolVar(&opt.dryrun)
	app.Arg("path", "target file path").Required().ExistingFileVar(&opt.path)

	if _, err := app.Parse(os.Args[1:]); err != nil {
		app.FatalUsage(err.Error())
	}
	if err := run(opt.path, opt.dryrun); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run(path string, dryrun bool) error {
	return errors.New("hmm")
}
