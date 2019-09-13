package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"

	"github.com/yudai/gojsondiff"
	"github.com/yudai/gojsondiff/formatter"
)

func main() {
	d := gojsondiff.New()
	config := formatter.AsciiFormatterConfig{
		ShowArrayIndex: true,
		Coloring:       true,
	}

	{
		x, _ := ioutil.ReadFile("00x.json")
		y, _ := ioutil.ReadFile("00y.json")

		diff, err := d.Compare(x, y)
		//pp.Println(diff, err)

		var xob map[string]interface{}
		json.Unmarshal(x, &xob)
		formatter := formatter.NewAsciiFormatter(xob, config)

		s, err := formatter.Format(diff)
		fmt.Println(s, err)
	}

	fmt.Println("----------------------------------------")
	{
		x, _ := ioutil.ReadFile("00x.json")
		z, _ := ioutil.ReadFile("00z.json")

		diff, err := d.Compare(x, z)
		// pp.Println(diff, err)

		var xob map[string]interface{}
		json.Unmarshal(x, &xob)
		formatter := formatter.NewAsciiFormatter(xob, config)

		s, err := formatter.Format(diff)
		fmt.Println(s, err)
	}
	// fmt.Println("----------------------------------------")
	// {
	// 	x, _ := ioutil.ReadFile("00x.json")
	// 	a, _ := ioutil.ReadFile("00a.json")

	// 	diff, err := d.Compare(x, a)
	// 	pp.Println(diff, err)

	// 	var xob map[string]interface{}
	// 	json.Unmarshal(x, &xob)
	// 	formatter := formatter.NewAsciiFormatter(xob, config)

	// 	s, err := formatter.Format(diff)
	// 	fmt.Println(s, err)
	// }
}
