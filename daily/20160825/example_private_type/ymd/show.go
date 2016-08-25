package ymd

import (
	"fmt"
)

// ShowJa is show message for japanese
func ShowJa(m Month, d Day) string {
	return fmt.Sprintf("%d月%d日", m, d)
}
