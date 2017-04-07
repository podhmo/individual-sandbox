package main

func main() {
	q := make(chan struct{})
	close(q)
	q <- struct{}{} // panic
}
