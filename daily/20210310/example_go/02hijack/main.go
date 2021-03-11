package main

// https://gist.github.com/Soulou/6048212

import (
	"crypto/tls"
	"fmt"
	"net"
	"net/http"
	"net/http/httputil"
	"time"
)

func main() {
	http.HandleFunc("/auth", func(res http.ResponseWriter, req *http.Request) {
		conn, _, err := res.(http.Hijacker).Hijack()
		if err != nil {
			panic(err)
		}
		conn.Write([]byte{})
		fmt.Fprintf(conn, "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\n")

		buffer := make([]byte, 1024)
		fmt.Println("Server : Enter routine")
		for {
			time.Sleep(1 * time.Second)
			fmt.Println("Server : I send")
			_, err = conn.Write([]byte("Hijack server"))
			if err != nil {
				panic(err)
			}
			fmt.Println("Server : I'm receiving")
			n, err := conn.Read(buffer)
			if err != nil {
				panic(err)
			}
			fmt.Printf("Server : %d bytes from client : %s\n", n, string(buffer))
		}
	})

	go runClient()

	err := http.ListenAndServeTLS(":8081", "./server.crt", "./server.key", nil)
	if err != nil {
		panic(err)
	}
}

func runClient() {
	time.Sleep(1 * time.Second)
	req, err := http.NewRequest("GET", "/auth", nil)
	if err != nil {
		panic(err)
	}

	dial, err := net.Dial("tcp", "localhost:8081")
	if err != nil {
		panic(err)
	}
	fmt.Println("Client : create TLS connection")
	tls_conn := tls.Client(dial, &tls.Config{InsecureSkipVerify: true})

	fmt.Println("Client : create http connection from tls client")
	conn := httputil.NewClientConn(tls_conn, nil)

	fmt.Println("Client : do request through http connection")
	_, err = conn.Do(req)
	if err != httputil.ErrPersistEOF && err != nil {
		panic(err)
	}

	fmt.Println("Client : hijack https connection")
	connection, reader := conn.Hijack()

	buffer := make([]byte, 1024)
	fmt.Println("Client : Enter client routine")
	for {
		time.Sleep(250 * time.Millisecond)
		n, err := reader.Read(buffer)
		if err != nil {
			panic(err)
		}
		fmt.Printf("Receive %n bytes : %s\n", n, string(buffer))
		connection.Write([]byte("I am Leo"))
	}
}
