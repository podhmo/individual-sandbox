package main

import (
	"fmt"
	"go/build"
	"io/ioutil"
	"log"
	"os"
	"time"

	"golang.org/x/tools/go/loader"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
	if err := run2(); err != nil {
		log.Fatalf("%+v", err)
	}
	if err := run10(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func newLoader(callback func(path string)) *loader.Config {
	ctxt := build.Default
	ctxt.ReadDir = func(path string) ([]os.FileInfo, error) {
		callback(path)
		return ioutil.ReadDir(path)
	}

	c := &loader.Config{
		TypeCheckFuncBodies: func(path string) bool {
			return false
		},
		Build: &ctxt,
	}
	c.TypeChecker.DisableUnusedImportCheck = true
	return c
}

func run() error {
	st := time.Now()
	defer func() {
		log.Println("time", time.Now().Sub(st))
	}()

	i := 0
	c := newLoader(func(path string) { i++ })
	c.Import("fmt")
	if _, err := c.Load(); err != nil {
		return err
	}
	fmt.Println("load directory", i)

	return nil
}

func run2() error {
	st := time.Now()
	defer func() {
		log.Println("time", time.Now().Sub(st))
	}()

	i := 0

	{
		c := newLoader(func(path string) { i++ })
		c.Import("fmt")
		if _, err := c.Load(); err != nil {
			return err
		}
	}
	{
		c := newLoader(func(path string) { i++ })
		c.Import("fmt")
		if _, err := c.Load(); err != nil {
			return err
		}
	}
	fmt.Println("load directory2", i)

	return nil
}

func run10() error {
	st := time.Now()
	defer func() {
		log.Println("time", time.Now().Sub(st))
	}()

	i := 0

	{
		c := newLoader(func(path string) { i++ })
		c.Import("fmt")
		if _, err := c.Load(); err != nil {
			return err
		}
	}
	{
		c := newLoader(func(path string) { i++ })
		c.Import("fmt")
		if _, err := c.Load(); err != nil {
			return err
		}
	}
	{
		c := newLoader(func(path string) { i++ })
		c.Import("fmt")
		if _, err := c.Load(); err != nil {
			return err
		}
	}
	{
		c := newLoader(func(path string) { i++ })
		c.Import("fmt")
		if _, err := c.Load(); err != nil {
			return err
		}
	}
	{
		c := newLoader(func(path string) { i++ })
		c.Import("fmt")
		if _, err := c.Load(); err != nil {
			return err
		}
	}
	{
		c := newLoader(func(path string) { i++ })
		c.Import("fmt")
		if _, err := c.Load(); err != nil {
			return err
		}
	}
	{
		c := newLoader(func(path string) { i++ })
		c.Import("fmt")
		if _, err := c.Load(); err != nil {
			return err
		}
	}
	{
		c := newLoader(func(path string) { i++ })
		c.Import("fmt")
		if _, err := c.Load(); err != nil {
			return err
		}
	}
	{
		c := newLoader(func(path string) { i++ })
		c.Import("fmt")
		if _, err := c.Load(); err != nil {
			return err
		}
	}
	{
		c := newLoader(func(path string) { i++ })
		c.Import("fmt")
		if _, err := c.Load(); err != nil {
			return err
		}
	}
    
	fmt.Println("load directory10", i)

	return nil
}
