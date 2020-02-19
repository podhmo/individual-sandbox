package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"net"
	"strings"
	"sync"

	"github.com/k0kubun/pp"
)

func main() {
	l, _ := net.Listen("tcp", ":8888")
	defer l.Close()

	var wg sync.WaitGroup
	wg.Add(1)

	// Wait for a connection.
	conn, _ := l.Accept()
	go func(c net.Conn) {
		defer wg.Done()

		for {
			text, err := bufio.NewReader(conn).ReadString('\n')
			if err != nil {
				if err == io.EOF {
					break
				}
				log.Fatal(err)
			}
			pp.Println("<-", text)
			msg := strings.Trim(text, "\r\n")
			if msg == "" {
				break
			}
			fmt.Fprintf(conn, "@%s@\n", msg)
		}
		//connection closed
		c.Close()
	}(conn)
	wg.Wait()
	fmt.Println("<- ok")
}
