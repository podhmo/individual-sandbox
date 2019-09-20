package main

import (
	"fmt"
	"io/ioutil"

	"github.com/nsf/jsondiff"
)

func main() {
	{
		x, _ := ioutil.ReadFile("00x.json")
		y, _ := ioutil.ReadFile("00y.json")

		options := jsondiff.DefaultConsoleOptions()
		diff, err := jsondiff.Compare(x, y, &options)
		fmt.Println(diff.String(), "@@@", err)
	}
	fmt.Println("----------------------------------------")
	{
		x, _ := ioutil.ReadFile("00x.json")
		z, _ := ioutil.ReadFile("00z.json")

		options := jsondiff.DefaultConsoleOptions()
		diff, err := jsondiff.Compare(x, z, &options)
		fmt.Println(diff.String(), "@@@", err)
	}
	fmt.Println("----------------------------------------")
	{
		x, _ := ioutil.ReadFile("00x.json")
		z, _ := ioutil.ReadFile("00z.json")

		options := jsondiff.DefaultConsoleOptions()
		diff, err := jsondiff.Compare(z, x, &options)
		fmt.Println(diff.String(), "@@@", err)
	}
	fmt.Println("----------------------------------------")
	{
		x, _ := ioutil.ReadFile("00x.json")
		z, _ := ioutil.ReadFile("00a.json")

		options := jsondiff.DefaultConsoleOptions()
		diff, err := jsondiff.Compare(x, z, &options)
		fmt.Println(diff.String(), "@@@", err)
	}
}
