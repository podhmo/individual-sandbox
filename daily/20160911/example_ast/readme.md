```bash
$ go run 01*
2016/09/11 23:14:40 before:
package main

type U struct {
}

func (u *U) Greeting() string {
	return "hello"
}
2016/09/11 23:14:40 after:
package main

type T struct {
}

func (u *T) Greeting() string {
	return "hello"
}
```
