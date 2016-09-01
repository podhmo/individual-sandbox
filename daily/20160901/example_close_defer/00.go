package main

import (
	"bufio"
	"fmt"
	"github.com/pkg/errors"
	"io/ioutil"
	"os"
)

func write(w *bufio.Writer) {
	w.WriteString("hai")
	w.Flush()
}

// almost os.File but Close() method occurs error.
type mockFD struct {
	*os.File
}

func (m *mockFD) Close() error {
	return errors.New("oops")
}

func writeFile() (outname string, err error){
	fd, err := ioutil.TempFile("", "test-")
	mfd := &mockFD{File: fd}
	if err != nil {
		return "", errors.Wrap(err, ":")
	}

	write(bufio.NewWriter(mfd))

	defer func() {
		// close maybe failured, sometimes?(checking is needed.)
		if cerr := mfd.Close(); cerr != nil {
			err = cerr
		}
	}()
	return mfd.Name(), err
}

func readFile(name string) error {
	buf, err := ioutil.ReadFile(name)
	if err != nil {
		return errors.Wrap(err, ":")
	}
	fmt.Println("data: ", string(buf))
	return nil
}

func doSomething() error {
	name, err := writeFile()
	if err != nil {
		return err
	}
	fmt.Println("wrote.. ", name)
	err = readFile(name)
	if err != nil {
		return err
	}
	return nil
}

func main() {
	err := doSomething()
	if err != nil {
		fmt.Printf("%+v\n", err)
		os.Exit(1)
	}
}
