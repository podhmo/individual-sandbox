package main

import (
	"fmt"
	"os"
)

func main() {
	// fp, err := ioutil.TempDir("", "")
	fmt.Println(os.TempDir())
}
