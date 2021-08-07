package main

import (
	"runtime/debug"
	"strings"
	"fmt"
)

func F(inner func()){
	name := "F"
	padding := strings.Repeat("  ", len(strings.Split(string(debug.Stack()), "\n")))
	fmt.Printf("%s%s start\n", padding, name)
	inner()
	fmt.Printf("%s%s end\n", padding, name)
}

func G(inner func()){
	name := "G"
	padding := strings.Repeat("  ", len(strings.Split(string(debug.Stack()), "\n")))
	fmt.Printf("%s%s start\n", padding, name)
	inner()
	fmt.Printf("%s%s end\n", padding, name)
}

func H(inner func()){
	name := "H"
	padding := strings.Repeat("  ", len(strings.Split(string(debug.Stack()), "\n")))
	fmt.Printf("%s%s start\n", padding, name)
	inner()
	fmt.Printf("%s%s end\n", padding, name)
}

func FGH(){
	name := "F"
	padding := strings.Repeat("  ", 1)
	fmt.Printf("%s%s start\n", padding, name)
	defer fmt.Printf("%s%s end\n", padding, name)
	{
		name := "G"
		padding := strings.Repeat("  ", 2)
		fmt.Printf("%s%s start\n", padding, name)
		defer fmt.Printf("%s%s end\n", padding, name)
		{
			name := "H"
			padding := strings.Repeat("  ", 3)
			fmt.Printf("%s%s start\n", padding, name)
			defer fmt.Printf("%s%s end\n", padding, name)
			{
				fmt.Println("Hello")
				// panic("hmm")
			}
		}
	}
}

func main(){
	F(func(){ G( func(){ H(func(){
		 fmt.Println("Hello");
		 // panic("hmm")
		})})})
	fmt.Println("----------------------------------------")
	FGH()
}

// func (s *Log) HandlerFunc(next http.Handler) http.Handler {
//     return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
//         s.Logger.Info("[START]LOG")
//         next.ServeHTTP(w, r)
//         s.Logger.Info("[END]LOG")
//     })
// }
