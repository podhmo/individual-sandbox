package main

import (
	"fmt"

	"github.com/wawandco/fako"
)

type User struct {
	Name     string `fako:"full_name"`
	Username string `fako:"username"`
	Email    string `fako:"email_address"` //Notice the fako:"email_address" tag
	Phone    string `fako:"phone"`
	Password string `fako:"simple_password"`
	Address  string `fako:"street_address"`
}

func main() {
	var user User
	fako.Fill(&user)

	fmt.Printf("fill: %+v\n", user)
	// This prints something like AnthonyMeyer@Twimbo.biz
	// or another valid email

	var userWithOnlyEmail User
	fako.FillOnly(&userWithOnlyEmail, "Email")
	fmt.Printf("wihtOnly Email: %+v\n", userWithOnlyEmail)
	//This will fill all only the email

	var userWithoutEmail User
	fako.FillExcept(&userWithoutEmail, "Email")
	//This will fill all the fields except the email
	fmt.Printf("wihtout Email: %+v\n", userWithoutEmail)
}
