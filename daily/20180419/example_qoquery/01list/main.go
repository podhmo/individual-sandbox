package main

import (
	"encoding/csv"
	"encoding/json"
	"log"
	"net/url"
	"os"
	"os/user"
	"path/filepath"
	"time"

	"github.com/ChimeraCoder/anaconda"
	kingpin "gopkg.in/alecthomas/kingpin.v2"
)

type opt struct {
	configPath  string
	userName    string
	listName    string
	targetFiles []string
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
	app := kingpin.New("create-list", "create list")

	app.Flag("config", "config file path").Short('c').ExistingFileVar(&opt.configPath)
	app.Flag("username", "user name").Short('u').StringVar(&opt.userName)
	app.Flag("listname", "list name").Required().StringVar(&opt.listName)
	app.Arg("targets", "target name list's files").ExistingFilesVar(&opt.targetFiles)

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

	if err := run(opt.configPath, opt.userName, opt.listName, opt.targetFiles); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run(path string, username string, listname string, targets []string) error {
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

	log.Println("get user..")
	u, err := api.GetUsersShow(username, nil)
	if err != nil {
		return err
	}
	log.Println("got user id is", u.Id)
	log.Println("get target list..")
	lists, err := api.GetListsOwnedBy(u.Id, nil)
	if err != nil {
		return err
	}
	var matched *anaconda.List
	for _, list := range lists {
		if list.Name == listname {
			log.Println("got target list", list.Name)
			matched = &list
			break
		}
	}
	if matched == nil {
		log.Println("target list is not found. create it...")
		v := url.Values{}
		v.Add("mode", "private") // default private
		list, err := api.CreateList(listname, listname, v)
		if err != nil {
			return err
		}
		log.Println("target list is created")
		matched = &list
	}

	for _, target := range targets {
		if err := func() error {
			log.Println("open", target)
			f, err := os.Open(target)
			if err != nil {
				return err
			}
			defer f.Close()
			r := csv.NewReader(f)

			records, err := r.ReadAll()
			if err != nil {
				return err
			}
			names := make([]string, len(records))
			for i := range records {
				names[i] = records[i][0]
			}
			for _, names := range chunk(names, 50) {
				if _, err := api.AddMultipleUsersToList(names, matched.Id, nil); err != nil {
					return err
				}
				log.Println("add users to list", target, len(names))
				time.Sleep(500 * time.Millisecond)
			}
			return nil
		}(); err != nil {
			return err
		}
	}
	return nil
}

func chunk(xs []string, n int) [][]string {
	var r [][]string

	for i := 0; i < len(xs); i += n {
		end := i + n
		if end > len(xs) {
			end = len(xs)
		}
		r = append(r, xs[i:end])
	}
	return r
}
