package main

import (
	"encoding/json"
	"time"

	multierror "github.com/hashicorp/go-multierror"
	"github.com/pkg/errors"
)

// FormatCheck : (generated from ..Person)
func (x *Person) FormatCheck() error {
	var merr *multierror.Error

	if len(x.Name) > 255 {
		merr = multierror.Append(merr, errors.New("name maxLength"))
	}
	if len(x.Name) < 1 {
		merr = multierror.Append(merr, errors.New("name minLength"))
	}
	if len(x.NickName) > 255 {
		merr = multierror.Append(merr, errors.New("nickname maxLength"))
	}
	if x.Age > 200 {
		merr = multierror.Append(merr, errors.New("age max"))
	}
	if x.Age < 0 {
		merr = multierror.Append(merr, errors.New("age min"))
	}
	if x.Father != nil {
		if err := x.Father.FormatCheck(); err != nil {
			merr = multierror.Append(merr, errors.WithMessage(err, "Father"))
		}
	}
	if x.Mother != nil {
		if err := x.Mother.FormatCheck(); err != nil {
			merr = multierror.Append(merr, errors.WithMessage(err, "Mother"))
		}
	}
	return merr.ErrorOrNil()
}

// UnmarshalJSON : (generated from ..Person)
func (x *Person) UnmarshalJSON(b []byte) error {
	type internal struct {
		Name      *string    `json:"name" minLength:"1" maxLength:"255"`
		NickName  *string    `json:"nickname" required:"false" maxLength:"255"`
		Age       *int       `json:"age" max:"200" min:"0"`
		CreatedAt *time.Time `json:"createdAt"`
		Father    **Person   `json:"father" required:"false"`
		Mother    **Person   `json:"mother" required:"false"`
	}

	var p internal
	if err := json.Unmarshal(b, &p); err != nil {
		return err
	}

	var merr *multierror.Error
	if p.Name == nil {
		merr = multierror.Append(merr, errors.New("name is required"))
	} else {
		x.Name = *p.Name
	}
	if p.NickName != nil {
		x.NickName = *p.NickName
	}
	if p.Age == nil {
		merr = multierror.Append(merr, errors.New("age is required"))
	} else {
		x.Age = *p.Age
	}
	if p.CreatedAt == nil {
		merr = multierror.Append(merr, errors.New("createdAt is required"))
	} else {
		x.CreatedAt = *p.CreatedAt
	}
	if p.Father != nil {
		x.Father = *p.Father
	}
	if p.Mother != nil {
		x.Mother = *p.Mother
	}
	if merr != nil {
		return merr.ErrorOrNil()
	}
	return x.FormatCheck()
}
