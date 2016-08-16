package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"github.com/google/go-github/github"
	"golang.org/x/oauth2"
	"io/ioutil"
	"log"
	"os"
	"path"
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
func AppMain(client *github.Client, filenames []string) {
	gist, err := NewGist(filenames)
	if err != nil {
		log.Fatal(err)
	}

	_, response, err := client.Gists.Create(gist)
	if err != nil {
		log.Fatal(err)
	}
	ppJSON(response)
}

// NewGist is shorthand of github.Gist object creation
func NewGist(filenames []string) (*github.Gist, error) {
	public := true
	files := make(map[github.GistFilename]github.GistFile)

	for _, filename := range filenames {
		gistfile, err := NewGistFile(filename)
		if err != nil {
			log.Printf("skip file=%s err=%v\n", filename, err)
			continue
		}
		k := github.GistFilename(path.Base(filename))
		files[k] = *gistfile
	}

	gist := github.Gist{
		Public: &public,
		Files:  files,
	}
	return &gist, nil
}

// NewGistFile is shorthand of github.GistFile object creation
func NewGistFile(filename string) (*github.GistFile, error) {
	basename := path.Base(filename)
	finfo, err := os.Stat(filename)
	if err != nil {
		return nil, err
	}
	size := int(finfo.Size())

	byte, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, err
	}
	content := string(byte)

	gistfile := github.GistFile{
		Size:     &size,
		Filename: &basename,
		Content:  &content,
	}
	return &gistfile, nil
}

func main() {
	if len(os.Args) <= 1 {
		fmt.Fprintf(os.Stderr, "selfish <token>\n")
		os.Exit(1)
	}
	token := os.Args[1]
	client := CreateClient(token)
	AppMain(client, os.Args[2:])
}
