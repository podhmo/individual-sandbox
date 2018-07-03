package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"os/exec"
	"strings"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run() error {
	xs := []int{1, 2, 3, 4, 5}
	_ = xs
	cmd := exec.Command("go", "run", "../03server/main.go")
	r, err := cmd.StdoutPipe()
	if err != nil {
		return err
	}

	defer func() {
		if cmd != nil && cmd.Process != nil {
			cmd.Process.Kill()
		}
	}()
	if err := cmd.Start(); err != nil {
		return err
	}

	br := bufio.NewReader(r)
	b, _, err := br.ReadLine()
	if err != nil {
		return err
	}

	firstLine := string(b)
	port := strings.TrimSuffix(firstLine, "\n")

	url := fmt.Sprintf("http://localhost:%s/", port)
	fmt.Println("request", url)
	response, err := http.Get(url)
	if err != nil {
		return err
	}
	defer response.Body.Close()

	val := map[string]string{}
	decoder := json.NewDecoder(response.Body)
	if err := decoder.Decode(&val); err != nil {
		return err
	}
	return nil
}
