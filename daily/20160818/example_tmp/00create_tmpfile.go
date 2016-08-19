package main

import (
	"io/ioutil"
	"bufio"
	"fmt"
)

func main(){
    // ./<tmp-file>
    {
        fp, err := ioutil.TempFile(".", "tmp-")
        if err != nil {
            panic(err)
        }
        defer fp.Close()

        fmt.Printf("output to %s\n", fp.Name())
        w := bufio.NewWriter(fp)
        w.WriteString("hello\n")
        w.Flush()
    }
    // <tmp-directory>/<tmp-file>
    {
        fp, err := ioutil.TempFile("", "tmp-")
        if err != nil {
            panic(err)
        }
        defer fp.Close()

        fmt.Printf("output to %s\n", fp.Name())
        w := bufio.NewWriter(fp)
        w.WriteString("hello\n")
        w.Flush()
    }
}
