package ymd

import (
	"fmt"
)

// Month is restricted int for month's expression(1 <= x <= 12)
type Month interface {
	Month() int
}

// Day is restricted int for day's expression(1 <= x <= 31)
type Day interface {
	Day() int
}

type month int
type day int

func (m month) Month() int {
	return int(m)
}

func (d day) Day() int {
	return int(d)
}

// NewMonth is factory of Month
func NewMonth(m int) (Month, error) {
	if m < 1 || 12 < m {
		return nil, fmt.Errorf("out of range must be 1 <= x <= 12")
	}
    return month(m), nil
}

// NewDay is factory of Day
func NewDay(d int) (Day, error) {
    // 本当はまじめに制限しないとダメ
	if d < 1 || 31 < d {
		return nil, fmt.Errorf("out of range must be 1 <= x <= 31")
	}
    return day(d), nil
}
