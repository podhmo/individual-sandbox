package main

func init() { main() }
func init() { panic() }

var panic = main

func main() { print("A") }
func init() { main() }
