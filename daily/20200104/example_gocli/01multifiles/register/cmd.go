package register

import (
	"fmt"

	kingpin "gopkg.in/alecthomas/kingpin.v2"
)

var (
	// Cmd ...
	Cmd = kingpin.Command("register", "Register a new user.")

	nickname = Cmd.Flag("nickname", "Nickname for user.").Default("anonymous").String()
	name     = Cmd.Arg("name", "Name of user.").Required().String()
)

// Run ...
func Run() error {
	name := *name
	nickname := *nickname
	fmt.Println("register, name=", name, "nickname=", nickname)
	return nil
}
