package calendar

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestDayOfX(t *testing.T) {
	/*
	        12月 2016
	   日 月 火 水 木 金 土
	                1  2  3
	    4  5  6  7  8  9 10
	   11 12 13 14 15 16 17
	   18 19 20 21 22 23 24
	   25 26 27 28 29 30 31
	*/
	type pair struct {
		day      Date
		expected Date
	}
	t.Run("first day of week", func(t *testing.T) {
		candidates := []pair{
			{day: MustDate("2016-12-14"), expected: MustDate("2016-12-12")},
			{day: MustDate("2016-12-12"), expected: MustDate("2016-12-12")},
			{day: MustDate("2016-12-11"), expected: MustDate("2016-12-05")},
		}
		for _, c := range candidates {
			t.Run(c.day.String(), func(t *testing.T) {
				assert.Exactly(t, c.expected, c.day.FirstDayOfWeek())
			})
		}
	})
	t.Run("last day of week", func(t *testing.T) {
		candidates := []pair{
			{day: MustDate("2016-12-14"), expected: MustDate("2016-12-18")},
			{day: MustDate("2016-12-18"), expected: MustDate("2016-12-18")},
			{day: MustDate("2016-12-19"), expected: MustDate("2016-12-25")},
		}
		for _, c := range candidates {
			t.Run(c.day.String(), func(t *testing.T) {
				assert.Exactly(t, c.expected, c.day.LastDayOfWeek())
			})
		}
	})
	t.Run("first day of month", func(t *testing.T) {
		candidates := []pair{
			{day: MustDate("2016-12-14"), expected: MustDate("2016-12-01")},
			{day: MustDate("2016-12-01"), expected: MustDate("2016-12-01")},
			{day: MustDate("2016-11-30"), expected: MustDate("2016-11-01")},
			{day: MustDate("2016-10-31"), expected: MustDate("2016-10-01")},
		}
		for _, c := range candidates {
			t.Run(c.day.String(), func(t *testing.T) {
				assert.Exactly(t, c.expected, c.day.FirstDayOfMonth())
			})
		}
	})
	t.Run("last day of month", func(t *testing.T) {
		candidates := []pair{
			{day: MustDate("2016-12-14"), expected: MustDate("2016-12-31")},
			{day: MustDate("2016-12-31"), expected: MustDate("2016-12-31")},
			{day: MustDate("2017-01-01"), expected: MustDate("2017-01-31")},
			{day: MustDate("2016-11-30"), expected: MustDate("2016-11-30")},
			{day: MustDate("2016-10-31"), expected: MustDate("2016-10-31")},
			{day: MustDate("2016-02-29"), expected: MustDate("2016-02-29")},
			{day: MustDate("2017-02-28"), expected: MustDate("2017-02-28")},
		}
		for _, c := range candidates {
			t.Run(c.day.String(), func(t *testing.T) {
				assert.Exactly(t, c.expected, c.day.LastDayOfMonth())
			})
		}
	})
}
