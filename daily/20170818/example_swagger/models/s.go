package models

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	strfmt "github.com/go-openapi/strfmt"

	"github.com/go-openapi/errors"
	"github.com/go-openapi/swag"
)

// S s
// swagger:model s
type S struct {

	// nperson
	Nperson *Nperson `json:"nperson,omitempty"`

	// person
	Person *Person `json:"person,omitempty"`
}

// Validate validates this s
func (m *S) Validate(formats strfmt.Registry) error {
	var res []error

	if err := m.validatePerson(formats); err != nil {
		// prop
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (m *S) validatePerson(formats strfmt.Registry) error {

	if swag.IsZero(m.Person) { // not required
		return nil
	}

	if m.Person != nil {

		if err := m.Person.Validate(formats); err != nil {
			if ve, ok := err.(*errors.Validation); ok {
				return ve.ValidateName("person")
			}
			return err
		}
	}

	return nil
}