package main

import (
	"fmt"
	"log"
	"net"
	"net/http"
	_ "net/http/pprof"
	"os"
	"runtime"
	"runtime/pprof"
)

// DefaultPort is default port for profiler.
var DefaultPort = ":55555"
var arr [][]int
var i = 0

func peek(w http.ResponseWriter, r *http.Request) {
	n := len(arr)
	fmt.Println("peek", n, "cap", cap(arr))
	fmt.Fprintf(w, `{"size": %d}`, n)
}

func gc(w http.ResponseWriter, r *http.Request) {
	fmt.Println("gc", len(arr))
	{
		fmt.Println("before")
		f, err := os.Create(fmt.Sprintf("01mem/%04dmem.txt", i))
		if err != nil {
			panic(err)
		}
		i++
		pprof.Lookup("heap").WriteTo(f, 1)
	}
	runtime.GC()
	fmt.Println("----------------------------------------")
	{

		fmt.Println("after")
		f, err := os.Create(fmt.Sprintf("01mem/%04dmem.txt", i))
		if err != nil {
			panic(err)
		}
		i++
		pprof.Lookup("heap").WriteTo(f, 1)
	}
}

func inc(w http.ResponseWriter, r *http.Request) {
	n := len(arr)
	fmt.Println("inc", n, "to", n+1, "cap", cap(arr))
	arr = append(arr, make([]int, 1000))
	fmt.Fprintf(w, `{"size": %d}`, len(arr))
}

func clear(w http.ResponseWriter, r *http.Request) {
	fmt.Println("clear", len(arr), "cap", cap(arr))
	arr = nil
	fmt.Fprintf(w, `{"size": %d}`, len(arr))
}

func setup() {
	runtime.SetBlockProfileRate(1)
	l, err := net.Listen("tcp", DefaultPort)
	if err != nil {
		l, err = net.Listen("tcp", "0.0.0.0:0")
		if err != nil {
			panic(err)
		}
	}
	go func() {
		log.Printf("start profiler: %s\n", l.Addr())
		if err := http.Serve(l, nil); err != nil {
			log.Fatal(err.Error())
		}
	}()
}

func run() {
	http.HandleFunc("/inc", inc)
	http.HandleFunc("/gc", gc)
	http.HandleFunc("/clear", clear)
	http.HandleFunc("/", peek)
	log.Printf("start server: %s\n", ":8080")
	http.ListenAndServe(":8080", nil)
}

func main() {
	// oldRate := runtime.MemProfileRate
	// runtime.MemProfileRate = 1
	// defer func() {
	// 	runtime.MemProfileRate = oldRate
	// }()

	// setup()
	run()
}
