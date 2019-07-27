package walk

import "strconv"

// Walk :
func Walk(
	v interface{},
	callback func(path []string, v interface{}),
) {
	var path []string
	walk(path, v, callback)
}

func walk(
	path []string,
	iface interface{},
	callback func(path []string, v interface{}),
) {
	switch val := iface.(type) {
	case map[string]interface{}:
		for k, v := range val {
			walk(append(path, k), v, callback)
		}
	case []interface{}:
		for i := range val {
			walk(append(path, strconv.Itoa(i)), val[i], callback)
		}
	default:
		callback(path, val)
	}
}
