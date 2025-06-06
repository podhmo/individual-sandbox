package booktestnested

import (
	"database/sql/driver"
	"fmt"
	"time"
)

type BookType string

const (
	BookTypeFICTION    BookType = "FICTION"
	BookTypeNONFICTION BookType = "NONFICTION"
)

func (e *BookType) Scan(src interface{}) error {
	switch s := src.(type) {
	case []byte:
		*e = BookType(s)
	case string:
		*e = BookType(s)
	default:
		return fmt.Errorf("unsupported scan type for BookType: %T", src)
	}
	return nil
}

type NullBookType struct {
	BookType BookType
	Valid    bool // Valid is true if BookType is not NULL
}

// Scan implements the Scanner interface.
func (ns *NullBookType) Scan(value interface{}) error {
	if value == nil {
		ns.BookType, ns.Valid = "", false
		return nil
	}
	ns.Valid = true
	return ns.BookType.Scan(value)
}

// Value implements the driver Valuer interface.
func (ns NullBookType) Value() (driver.Value, error) {
	if !ns.Valid {
		return nil, nil
	}
	return string(ns.BookType), nil
}

type Author struct {
	AuthorID int32
	Name     string
}

type Book struct {
	BookID    int32
	AuthorID  int32
	Isbn      string
	BookType  BookType
	Title     string
	Year      int32
	Available time.Time
	Tags      []string
}
