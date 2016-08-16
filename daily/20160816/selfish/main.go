package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"github.com/google/go-github/github"
	"golang.org/x/oauth2"
	"log"
	"os"
)

func ppJSON(target interface{}) {
	b, err := json.Marshal(target)
	if err != nil {
		log.Fatal(err)
	}

	var out bytes.Buffer
	json.Indent(&out, b, " ", "    ")
	out.WriteTo(os.Stdout)
}

// CreateClient is factory of github client
func CreateClient(token string) *github.Client {
	ts := oauth2.StaticTokenSource(
		&oauth2.Token{AccessToken: token},
	)
	tc := oauth2.NewClient(oauth2.NoContext, ts)
	return github.NewClient(tc)
}

// AppMain is main function of Application
func AppMain(client *github.Client) {
	// list all repositories for the authenticated user
	repos, _, err := client.Repositories.List("", nil)

	if err != nil {
		log.Fatal(err)
	}
	ppJSON(repos)
}

func main() {
	if len(os.Args) <= 1 {
		fmt.Fprintf(os.Stderr, "selfish <token>\n")
		os.Exit(1)
	}
	token := os.Args[1]
	client := CreateClient(token)
	AppMain(client)
}
