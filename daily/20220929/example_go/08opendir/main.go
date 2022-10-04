package main

import (
	"fmt"
	"os"
)

func main() {
	filename := os.Args[1]
	fmt.Println("open: ", filename)
	f, err := os.Open(filename)
	fmt.Printf("%+#v: %+v\n", f, err)
	if err != nil {
		return
	}
	defer f.Close()

	// write /dev/stdout: copy_file_range: is a directory

	// if _, err := io.Copy(os.Stdout, f); err != nil {
	// 	fmt.Println(err)
	// }
}
