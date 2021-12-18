package x01

type EmailAddress string

type RegisterInput struct {
	EmailAddress EmailAddress
	Plan         string // enum
}
type RegisterOutput struct{}
type DB interface {
	Save(interface{}) error
}

// action (usecase)
func Register(db DB, input RegisterInput) (*RegisterOutput, error) {
	// validate input
	ob, err := Verify(input)
	if err != nil {
		return nil, err
	}
	// internal save action
	return nil, register(db, ob)
}

func Verify(input RegisterInput) (VerifiedRegisterInput, error) {
	return VerifiedRegisterInput{}, nil
}

type VerifiedRegisterInput struct {
	RegisterInput
}

// command
func register(db DB, ob VerifiedRegisterInput) error {
	return db.Save(ob)
}
