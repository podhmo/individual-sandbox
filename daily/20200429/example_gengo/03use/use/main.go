package main
// this packaage is auto generated

import (
	"flag"
	"os"
	"log"
	"m/internal2"
)

// Option ...
type Option struct {
	Grumby bool // for `-grumby`
}


func main()  {
	opt := &Option{}
	cmd := flag.NewFlagSet("use", flag.ContinueOnError)

	cmd.BoolVar(&opt.Grumby, "grumby", false, "grumby? what is this?")

	if err := cmd.Parse(os.Args[1:]); err != nil {
		if err != flag.ErrHelp {
			cmd.Usage()
		}
		os.Exit(1)
	}
	if err := run(opt); err != nil {
		log.Fatalf("!!%+v", err)
	}
}

func run(opt *Option) error {
	v0 := internal2.NewMessage()
	v1 := internal2.NewGreeter(v0, opt.Grumby)
	v2, err := internal2.NewEvent(v1)
	if err != nil  {
		return err
	}
	v2.Start()
	return nil
}