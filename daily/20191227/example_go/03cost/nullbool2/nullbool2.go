package nullbool2

// NullBool ...
type NullBool struct {
	Bool  bool
	Valid bool
}

func (v NullBool) MarshalJSON() ([]byte, error) {
	if !v.Valid {
		return []byte("null"), nil
	}
	if v.Bool {
		return []byte("true"), nil
	}
	return []byte("false"), nil
}
