package val

var (
	sNull = []byte("null")
)

func Null() ([]byte, error) {
	return sNull, nil
}
func Null2() ([]byte, error) {
	return []byte("null"), nil
}
