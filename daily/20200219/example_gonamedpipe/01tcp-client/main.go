package main

import (
	"bufio"
	"fmt"
	"log"
	"net"
	"os"
	"strings"

	"github.com/k0kubun/pp"
)

func main() {

	conn, _ := net.Dial("tcp", "127.0.0.1:8888")
	for {
		fmt.Print("Command to send: ")
		reader := bufio.NewReader(os.Stdin)
		text, err := reader.ReadString('\n')

		if err != nil {
			log.Fatal(err)
		}
		pp.Println("->", text)
		fmt.Fprintf(conn, "%s\r\n", text)
		b := make([]byte, 512)
		if strings.Trim(text, "\r\n") == "" {
			break
		}
		conn.Read(b)
		fmt.Println(string(b))
	}
	fmt.Println("-> ok")
}
