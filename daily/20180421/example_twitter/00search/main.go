package main

import (
	"encoding/json"
	"fmt"
	"log"
	"net/url"
	"os"
	"os/user"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/ChimeraCoder/anaconda"
	kingpin "gopkg.in/alecthomas/kingpin.v2"
)

type opt struct {
	configPath string
	keyword    string
	maxid      string
}

// config :
type config struct {
	ConsumerKey       string `json:"ConsumerKey"`
	ConsumerSecret    string `json:"ConsumerSecret"`
	AccessToken       string `json:"AccessToken"`
	AccessTokenSecret string `json:"AccessTokenSecret"`
}

func main() {
	var opt opt
	app := kingpin.New("search", "search")

	app.Flag("config", "config file path").Short('c').ExistingFileVar(&opt.configPath)
	app.Flag("max-id", "max id").StringVar(&opt.maxid)
	app.Arg("keyword", "keyword").Required().StringVar(&opt.keyword)

	if _, err := app.Parse(os.Args[1:]); err != nil {
		app.FatalUsage(err.Error())
	}

	u, err := user.Current()
	if err != nil {
		log.Fatal(err)
	}

	if opt.configPath == "" {
		dir := filepath.Join(u.HomeDir, ".config/egoist") // xxx
		opt.configPath = filepath.Join(dir, "config.json")
		log.Println("guessing confing path", opt.configPath)
	}

	if err := run(opt.configPath, opt.keyword, opt.maxid); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run(path string, keyword string, maxid string) error {
	var c config
	f, err := os.Open(path)
	if err != nil {
		return err
	}
	decoder := json.NewDecoder(f)
	if err := decoder.Decode(&c); err != nil {
		return err
	}

	api := anaconda.NewTwitterApiWithCredentials(
		c.AccessToken,
		c.AccessTokenSecret,
		c.ConsumerKey,
		c.ConsumerSecret,
	)
	v := make(url.Values)
	v.Add("locale", "ja")
	v.Add("result_type", "mixed")
	v.Add("count", "50")
	if maxid != "" {
		if strings.HasPrefix(maxid, `"`) && strings.HasSuffix(maxid, `"`) {
			maxid, err = strconv.Unquote(maxid)
			if err != nil {
				return err
			}
		}
		v.Add("max_id", maxid)
	}
	list, err := api.GetSearch(keyword, v)
	if err != nil {
		return err
	}
	for _, tweet := range list.Statuses {
		fmt.Printf("%q,%q,%q,%q\n", tweet.User.ScreenName, tweet.IdStr, tweet.CreatedAt, tweet.FullText)
	}
	return nil
}
