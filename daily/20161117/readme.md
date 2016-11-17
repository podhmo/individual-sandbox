# golang nilの扱い

```go
package main

import "fmt"

type I interface{}
type S struct{}

func f(v I) I {
	if v == nil {
		fmt.Printf("%[1]T: %[1]v -- nil\n", v)
	} else {
		fmt.Printf("%[1]T: %[1]v\n", v)
	}
	return v
}
func g(v *S) I {
	if v == nil {
		fmt.Printf("%[1]T: %[1]v -- nil\n", v)
	} else {
		fmt.Printf("%[1]T: %[1]v\n", v)
	}
	return v
}

func main() {
	fmt.Println("----------------------------------------")
	{
		var i I
		f(i)

	}
	fmt.Println("----------------------------------------")
	{
		var s *S
		f(s)
	}
	fmt.Println("----------------------------------------")
	{
		var s *S
		g(s)
	}
}
```

```
----------------------------------------
<nil>: <nil> -- nil
----------------------------------------
*main.S: <nil>
----------------------------------------
*main.S: <nil> -- nil
```
