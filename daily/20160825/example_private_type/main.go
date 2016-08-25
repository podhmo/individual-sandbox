package main

import (
	"./ymd"
	"log"
)

func doSomething(mval int, dval int) error {
	log.Println("----------------------------------------")
	log.Printf("input: %d, %d\n", mval, dval)
	log.Println("----------------------------------------")
	m, err := ymd.NewMonth(mval)
	if err != nil {
		return err
	}
	d, err := ymd.NewDay(dval)
	if err != nil {
		return err
	}
	log.Printf("%[1]s, %[1]v\n", m)
	log.Printf("%[1]s, %[1]v\n", d)
	log.Println(ymd.ShowJa(m, d))
	return nil
}

func main() {
	{
		err := doSomething(5, 3)
		if err != nil {
			log.Fatal(err)
		}
	}
	{
		err := doSomething(-5, 3)
		if err != nil {
			log.Println(err)
		}
	}
	{
		err := doSomething(5, 40)
		if err != nil {
			log.Println(err)
		}
	}
}
