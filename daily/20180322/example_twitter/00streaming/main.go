package main

import (
	"encoding/json"
	"flag"
	"log"
	"os"

	"github.com/ChimeraCoder/anaconda"
	"github.com/gen2brain/beeep"
)

// Config :
type Config struct {
	ConsumerKey       string `json:"ConsumerKey"`
	ConsumerSecret    string `json:"ConsumerSecret"`
	AccessToken       string `json:"AccessToken"`
	AccessTokenSecret string `json:"AccessTokenSecret"`
}

func main() {
	cPath := flag.String("conf", "config.json", "API Config File")
	flag.Parse()
	if *cPath == "" {
		flag.Usage()
		os.Exit(1)
	}
	if err := run(*cPath); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run(path string) error {
	var c Config
	f, err := os.Open(path)
	if err != nil {
		return err
	}
	decoder := json.NewDecoder(f)
	if err := decoder.Decode(&c); err != nil {
		return err
	}

	anaconda.SetConsumerKey(c.ConsumerKey)
	anaconda.SetConsumerSecret(c.ConsumerSecret)
	api := anaconda.NewTwitterApiWithCredentials(
		c.AccessToken,
		c.AccessTokenSecret,
		c.ConsumerKey,
		c.ConsumerSecret,
	)

	twitterStream := api.UserStream(nil)
	for {
		x := <-twitterStream.C
		switch tweet := x.(type) {
		case anaconda.Tweet:
			if err := beeep.Notify(tweet.User.Name, tweet.FullText, "assets/information.png"); err != nil {
				log.Printf("%+v", err)
			}
		case anaconda.StatusDeletionNotice:
			log.Println("deleted")
        default:
			log.Printf("unknown type(%T) : %v \n", x, x)
		}
	}
}
