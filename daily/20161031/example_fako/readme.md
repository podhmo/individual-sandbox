fakoというのがstructのfakerになっているっぽい

## setup

```bash
go install -v github.com/wawandco/fako
```
## run

```bash
$ go run 00*
fill: {Name:Cheryl Hayes Username: Email:tRamirez@Babbleblab.name Phone:4-737-934-90-28 Password:lUTtG98s1HF Address:Sutherland Pass 82}
wihtOnly Email: {Name: Username: Email:AlanHarris@Skyndu.biz Phone: Password: Address:}
wihtout Email: {Name:Justin Welch Username: Email: Phone:7-088-104-75-37 Password:wlOmi1q Address:Autumn Leaf Junction 53}
```

使い方はタグを付ける

```go
type User struct {
	Name     string `fako:"full_name"`
	Username string `fako:"username"`
	Email    string `fako:"email_address"` //Notice the fako:"email_address" tag
	Phone    string `fako:"phone"`
	Password string `fako:"simple_password"`
	Address  string `fako:"street_address"`
}

// in app
{
	var user User
    fako.Fill(&user)
}
```

使えるタグの種類

https://github.com/wawandco/fako に書いてある。
