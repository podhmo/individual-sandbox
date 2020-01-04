package post

import (
	"fmt"

	kingpin "gopkg.in/alecthomas/kingpin.v2"
)

var (
	// Cmd ...
	Cmd = kingpin.Command("post", "Post a message to a channel.")

	image   = Cmd.Flag("image", "Image to .").File()
	channel = Cmd.Arg("channel", "Channel to  to.").Required().String()
	text    = Cmd.Arg("text", "Text to .").Strings()
)

// Run ...
func Run() error {
	image := *image
	channel := *channel

	fmt.Println("post, image=", image, "channel=", channel)
	return nil
}
