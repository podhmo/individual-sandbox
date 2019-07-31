package internal

import (
	"fmt"
	"strings"
)

// URLJoin :
func URLJoin(x, y string) string {
	if x == "" {
		return y
	}
	if y == "" {
		return x
	}
	return fmt.Sprintf("%s/%s", strings.TrimSuffix(x, "/"), strings.TrimPrefix(y, "/"))
}
