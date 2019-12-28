package nullbool

// NullBool ...
type NullBool struct {
	Bool  bool
	Valid bool
}

func (v NullBool) MarshalJSON() ([]byte, error) {
	if !v.Valid {
		return sNull, nil
	}
	if v.Bool {
		return sTrue, nil
	}
	return sFalse, nil
}

const (
	sNull  = []byte("null")
	sTrue  = []byte("true")
	sFalse = []byte("false")
)
