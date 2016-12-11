package calendar

import (
	"fmt"
	"time"
)

// Date :
type Date time.Time

// DateFormat :
const DateFormat = "2006-01-02"

// MustDate : 文字列をDateに変換。変換できない文字列の場合にはpanic
func MustDate(text string) Date {
	date, err := time.Parse(DateFormat, text)
	if err != nil {
		panic(err)
	}
	return Date(date)
}

// String :
func (d Date) String() string {
	return d.Time().Format(DateFormat)
}

// Weekday :
func (d Date) Weekday() time.Weekday {
	return d.Time().Weekday()
}

// Time : time.Time
func (d Date) Time() time.Time {
	return time.Time(d)
}

// AddDays :
func (d Date) AddDays(days int) Date {
	return Date(d.Time().AddDate(0, 0, days))
}

// FirstDayOfWeek : first day of current week (Monday)
func (d Date) FirstDayOfWeek() Date {
	wday := d.Weekday()
	if wday == time.Monday {
		return d
	} else if wday == time.Sunday {
		return d.AddDays(-7 + 1)
	}
	return d.AddDays(-int(wday) + 1)
}

// LastDayOfWeek : last day of current week (Sunday)
func (d Date) LastDayOfWeek() Date {
	wday := d.Weekday()
	if wday == time.Sunday {
		return d
	}
	return d.AddDays(7 - int(wday))
}

// FirstDayOfMonth : first day of current month
func (d Date) FirstDayOfMonth() Date {
	return d.DayOfMonth(1)
}

// DayOfMonth : day of current month
func (d Date) DayOfMonth(i int) Date {
	return MustDate(fmt.Sprintf("%04d-%02d-%02d", d.Time().Year(), d.Time().Month(), i))
}

// LastDayOfMonth : last day of current month
func (d Date) LastDayOfMonth() Date {
	t := d.Time()
	m := t.Month()
	d2 := t.AddDate(0, 1, 0) // day of next month
	for {
		d2 = d2.AddDate(0, 0, -d2.Day())
		if m == d2.Month() {
			break
		}
	}
	return Date(d2)
}
