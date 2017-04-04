package main

import (
	"fmt"
	"regexp"
	"strings"
)

func main() {
	{
		text := "/Form/Order/OrderShipping.aspx"
		pattern := "/Form/(Order|Login|Error|User)"
		rx := regexp.MustCompile(pattern)
		b := rx.MatchString(strings.ToLower(text))
		fmt.Println(text, pattern, b)
	}
	{
		text := "/Form/Order/OrderShipping.aspx"
		pattern := "/Form/(order|login|error|user)"
		rx := regexp.MustCompile(pattern)
		b := rx.MatchString(strings.ToLower(text))
		fmt.Println(text, pattern, b)
	}
	{
		text := "/Form/Order/OrderShipping.aspx"
		pattern := "/form/(order|login|error|user)"
		rx := regexp.MustCompile(pattern)
		b := rx.MatchString(strings.ToLower(text))
		fmt.Println(text, pattern, b)
	}
	{
		text := "/Form/Order/OrderShipping.aspx"
		pattern := "/Form/(Order|Login|Error|User)"
		rx := regexp.MustCompile("(?i)" + pattern)
		b := rx.MatchString(text)
		fmt.Println(text, pattern, b)
	}
	{
		text := "/Form/Order/OrderShipping.aspx"
		pattern := "/Form/(order|login|error|user)"
		rx := regexp.MustCompile("(?i)" + pattern)
		b := rx.MatchString(text)
		fmt.Println(text, pattern, b)
	}
	{
		text := "/Form/Order/OrderShipping.aspx"
		pattern := "/form/(order|login|error|user)"
		rx := regexp.MustCompile("(?i)" + pattern)
		b := rx.MatchString(text)
		fmt.Println(text, pattern, b)
	}
}
