package add

import (
	"fmt"
	"math/big"
	"strings"

	kingpin "gopkg.in/alecthomas/kingpin.v2"
)

// SetupCommand :
func SetupCommand(app *kingpin.Application) (string, func() error) {
	cmd := app.Command("add", "add numbers")

	ns := cmd.Arg("ns", "numbers").Int64List()
	return cmd.FullCommand(), func() error {
		return Add(*ns...)
	}
}

// Add :
func Add(ns ...int64) error {
	var sum, x big.Int
	y := &sum
	for _, n := range ns {
		y = y.Add(y, x.SetInt64(n))
	}

	ss := make([]string, len(ns))
	for i := range ns {
		ss[i] = fmt.Sprintf("%v", ns[i])
	}
	fmt.Printf("%v = %s\n", sum, strings.Join(ss, " + "))
	return nil
}
