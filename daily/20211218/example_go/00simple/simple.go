package x00

type EmailAddress string
type VerifiedEmailAddress string

type RegisterInput struct {
	EmailAddress string
	Plan         string // enum
}

func Register(input RegisterInput) error {
	//  do something
	return nil
}
