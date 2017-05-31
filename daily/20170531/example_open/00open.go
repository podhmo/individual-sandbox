package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/user"
	"strings"
)

func cat(filename string) error {
	fp, err := os.Open(filename)
	if err != nil {
		return err
	}
	b, err := ioutil.ReadAll(fp)
	if err != nil {
		return err
	}
	fmt.Println(string(b))
	return nil
}

func main() {
	{
		fmt.Println("~/.bashrc")
		if err := cat("~/.bashrc"); err != nil {
			fmt.Println("@@@", err)
		}
	}
	{
		filename := strings.Replace("~/.bashrc", "~", os.Getenv("HOME"), 1)
		fmt.Println(filename)
		if err := cat(filename); err != nil {
			fmt.Println("@@@", err)
		}
	}
	{
		u, err := user.Current()
		if err != nil {
			log.Fatal(err)
		}
		filename := strings.Replace("~/.bashrc", "~", u.HomeDir, 1)
		fmt.Println(filename)
		if err := cat(filename); err != nil {
			fmt.Println("@@@", err)
		}
	}
}
