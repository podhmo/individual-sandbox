package main

import "fmt"

func main() {
	fmt.Println("panicOuter0", panicOuter0())
	fmt.Println("panicOuter1", panicOuter1())
	fmt.Println("panicInner0", panicInner0())
	fmt.Println("panicInner1", panicInner1())
	fmt.Println("notPanicOuter0", notPanicOuter0())
	fmt.Println("notPanicOuter1", notPanicOuter1())
	fmt.Println("notPanicInner0", notPanicInner0())
	fmt.Println("notPanicInner1", notPanicInner1())
}

func panicOuter0() (err error) {
	defer func() {
		r := recover()
		if r == nil {
			return
		}
		err = fmt.Errorf("%v", r)
	}()

	mustPanic()
	return
}

func panicOuter1() (err error) {
	defer func() {
		r := recover()
		if r == nil {
			return
		}
		err = fmt.Errorf("%v", r)
	}()

	mustPanic()
	return nil
}

func panicInner0() (err error) {
	func() {
		defer func() {
			r := recover()
			if r == nil {
				return
			}
			err = fmt.Errorf("%v", r)
		}()

		mustPanic()
	}()
	return
}

func panicInner1() (err error) {
	func() {
		defer func() {
			r := recover()
			if r == nil {
				return
			}
			err = fmt.Errorf("%v", r)
		}()

		mustPanic()
	}()
	return nil
}

func notPanicOuter0() (err error) {
	defer func() {
		err = fmt.Errorf("hmm..")
	}()
	return
}

func notPanicOuter1() (err error) {
	defer func() {
		err = fmt.Errorf("hmm..")
	}()
	return nil
}

func notPanicInner0() (err error) {
	func() {
		defer func() {
			err = fmt.Errorf("hmm..")
		}()
	}()
	return
}

func notPanicInner1() (err error) {
	func() {
		defer func() {
			err = fmt.Errorf("hmm..")
		}()
	}()
	return nil
}

func mustPanic() {
	type person struct {
		Name string
	}
	var p *person
	fmt.Println(p.Name) // nil panic
}
