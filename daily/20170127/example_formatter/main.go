package main

import (
	"fmt"
	"math"
	"strconv"
	"strings"
)

// FormatInt64 :
func FormatInt64(n int64) string {
	var prefix string
	if n > 0 {
		prefix = "+"
	}
	return fmt.Sprintf("%s%s", prefix, Comma(n))
}

// FormatFloat64 :
func FormatFloat64(n float64) string {
	var prefix string
	if n > 0 {
		prefix = "+"
	}
	return fmt.Sprintf("%s%.2f", prefix, n)
}

// from dustin/go-humanize

// Comma produces a string form of the given number in base 10 with
// commas after every three orders of magnitude.
//
// e.g. Comma(834142) -> 834,142
func Comma(v int64) string {
	sign := ""

	// minin64 can't be negated to a usable value, so it has to be special cased.
	if v == math.MinInt64 {
		return "-9,223,372,036,854,775,808"
	}

	if v < 0 {
		sign = "-"
		v = 0 - v
	}

	parts := []string{"", "", "", "", "", "", ""}
	j := len(parts) - 1

	for v > 999 {
		parts[j] = strconv.FormatInt(v%1000, 10)
		switch len(parts[j]) {
		case 2:
			parts[j] = "0" + parts[j]
		case 1:
			parts[j] = "00" + parts[j]
		}
		v = v / 1000
		j--
	}
	parts[j] = strconv.Itoa(int(v))
	return sign + strings.Join(parts[j:], ",")
}

func main() {
	fmt.Println(FormatInt64(100))
	fmt.Println(FormatInt64(-100))
	fmt.Println(FormatInt64(114514))
	fmt.Println(FormatInt64(-114514))
	fmt.Println(FormatFloat64(1.0245))
	fmt.Println(FormatFloat64(-1.0245))
	fmt.Println(FormatFloat64(100.0245))
	fmt.Println(FormatFloat64(-100.0245))
}
