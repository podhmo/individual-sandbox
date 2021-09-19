package main

import (
	"errors"
	"fmt"
	"time"
)

type Config struct {
	Prefix string
}

type Provider struct {
	Config *Config
}

func (p *Provider) Greeter() *Greeter {
	g := NewGreeter()
	g.Prefix = p.Config.Prefix
	return &g
}

// NewGreeter initializes a Greeter. If the current epoch time is an even
// number, NewGreeter will create a grumpy Greeter.
func NewGreeter() Greeter {
	var grumpy bool
	if time.Now().Unix()%2 == 0 {
		grumpy = true
	}
	return Greeter{Grumpy: grumpy}
}

// Greeter is the type charged with greeting guests.
type Greeter struct {
	Grumpy bool
	Prefix string
}

// Greet produces a greeting for guests.
func (g Greeter) Greet(message string) string {
	if g.Grumpy {
		return "Go away!"
	}
	return fmt.Sprintf("%s message", g.Prefix)
}

func RunEvent(greeter *Greeter, phrase string) error {
	if greeter.Grumpy {
		return errors.New("could not create event: event greeter is grumpy")
	}
	msg := greeter.Greet(phrase)
	fmt.Println(msg)
	return nil
}

func main() {
	provider := &Provider{
		Config: &Config{Prefix: "(o_0) ..."},
	}

	for i := 0; true; i++ {
		phrase := "hi there!"
		if err := run(provider, phrase); err != nil {
			fmt.Printf("failed to create event: %s\n", err)
			continue
		}
		break
	}
}

func run(p *Provider, phrase string) error {
	var greeter *Greeter
	{
		greeter = p.Greeter() // ここは変わる可能性がある (T, func()), (T, error), (T, func(), error)

		// 例えば...
		// var err error
		// greeter, err = p.GreeterWithError()
		// if err != nil {
		// 	return err
		// }
	}
	return RunEvent(greeter, phrase)
}
