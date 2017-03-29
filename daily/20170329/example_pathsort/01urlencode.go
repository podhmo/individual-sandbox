package main

import "fmt"

func urlencode(s string) (result string) {
	for _, c := range s {
		if c <= 0x7f { // single byte
			result += fmt.Sprintf("%%%X", c)
		} else if c > 0x1fffff { // quaternary byte
			result += fmt.Sprintf("%%%X%%%X%%%X%%%X",
				0xf0+((c&0x1c0000)>>18),
				0x80+((c&0x3f000)>>12),
				0x80+((c&0xfc0)>>6),
				0x80+(c&0x3f),
			)
		} else if c > 0x7ff { // triple byte
			result += fmt.Sprintf("%%%X%%%X%%%X",
				0xe0+((c&0xf000)>>12),
				0x80+((c&0xfc0)>>6),
				0x80+(c&0x3f),
			)
		} else { // double byte
			result += fmt.Sprintf("%%%X%%%X",
				0xc0+((c&0x7c0)>>6),
				0x80+(c&0x3f),
			)
		}
	}
	return result
}

func main() {
	fmt.Println("…", "->", urlencode("…"))
	fmt.Println("-", "->", urlencode("-"))
	fmt.Println("_", "->", urlencode("_"))
	fmt.Println(urlencode("_") < urlencode("-"))
	fmt.Println(urlencode("crossdomain-gtm/"), "<", urlencode("crossdomain/"), urlencode("crossdomain-gtm/") < urlencode("crossdomain/"))
}
