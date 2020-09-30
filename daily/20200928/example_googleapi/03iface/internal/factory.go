package internal

import (
	"context"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"

	"golang.org/x/oauth2"
	"golang.org/x/oauth2/google"
	"golang.org/x/xerrors"
	"google.golang.org/api/tasks/v1"
)

func NewTasksClientFactory(loadfile string, savefile string) *ClientFactory {
	return &ClientFactory{
		LoadFile: loadfile,
		SaveFile: savefile,

		GetConfig: func(filename string) (*oauth2.Config, error) {
			b, err := ioutil.ReadFile(filename)
			if err != nil {
				return nil, xerrors.Errorf("Unable to read client secret file: %w", err)
			}

			// If modifying these scopes, delete your previously saved token.json.
			config, err := google.ConfigFromJSON(b, tasks.TasksScope)
			if err != nil {
				return nil, xerrors.Errorf("Unable to parse client secret file to config: %w", err)
			}
			return config, nil
		},
	}
}

// ClientFactory is facade
type ClientFactory struct {
	GetConfig func(loadname string) (*oauth2.Config, error)
	LoadFile  string
	SaveFile  string

	config *oauth2.Config
	client *http.Client
}

func (f *ClientFactory) Client() *http.Client {
	if f.client != nil {
		return f.client
	}
	f.client = getClient(f.Config(), f.SaveFile)
	return f.client
}

func (f *ClientFactory) Config() *oauth2.Config {
	var err error

	if f.config != nil {
		return f.config
	}
	f.config, err = f.GetConfig(f.LoadFile)
	if err != nil {
		log.Fatal("get config: %+v", err) // xxx
	}
	return f.config
}

// Retrieve a token, saves the token, then returns the generated client.
func getClient(config *oauth2.Config, tokFile string) *http.Client {
	// The file token.json stores the user's access and refresh tokens, and is
	// created automatically when the authorization flow completes for the first
	// time.
	tok, err := tokenFromFile(tokFile)
	if err != nil {
		tok = getTokenFromWeb(config)
		saveToken(tokFile, tok)
	}
	return config.Client(context.Background(), tok)
}

// Request a token from the web, then returns the retrieved token.
func getTokenFromWeb(config *oauth2.Config) *oauth2.Token {
	authURL := config.AuthCodeURL("state-token", oauth2.AccessTypeOffline)
	fmt.Printf("Go to the following link in your browser then type the "+
		"authorization code: \n%v\n", authURL)

	var authCode string
	if _, err := fmt.Scan(&authCode); err != nil {
		log.Fatalf("Unable to read authorization code: %v", err)
	}

	tok, err := config.Exchange(context.TODO(), authCode)
	if err != nil {
		log.Fatalf("Unable to retrieve token from web: %v", err)
	}
	return tok
}

// Retrieves a token from a local file.
func tokenFromFile(file string) (*oauth2.Token, error) {
	f, err := os.Open(file)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	tok := &oauth2.Token{}
	err = json.NewDecoder(f).Decode(tok)
	return tok, err
}

// Saves a token to a file path.
func saveToken(path string, token *oauth2.Token) {
	fmt.Printf("Saving credential file to: %s\n", path)
	f, err := os.OpenFile(path, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0600)
	if err != nil {
		log.Fatalf("Unable to cache oauth token: %v", err)
	}
	defer f.Close()
	json.NewEncoder(f).Encode(token)
}
