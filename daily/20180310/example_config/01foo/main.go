package main

import (
	"log"
	"time"

	"github.com/podhmo/commithistory"
)

func main() {
	c := commithistory.New("foo")

	{
		var ob commithistory.Commit
		if err := c.LoadCommit("foo.history", "head", &ob); err != nil {
			log.Fatal(err)
		}
	}

	ob := commithistory.Commit{
		ID:        "68332035342c0cbd0ed6792e0869882c",
		Alias:     "head",
		CreatedAt: time.Now(),
		Action:    "create",
	}
	if err := c.SaveCommit("foo.history", &ob); err != nil {
		log.Fatal(err)
	}
}
