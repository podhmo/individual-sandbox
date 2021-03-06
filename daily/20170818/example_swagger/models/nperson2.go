package models

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	"github.com/go-openapi/errors"
	strfmt "github.com/go-openapi/strfmt"
	"github.com/go-openapi/swag"
)

// Nperson2 nperson2
// swagger:model nperson2
type Nperson2 struct {
	Person
}

// UnmarshalJSON unmarshals this object from a JSON structure
func (m *Nperson2) UnmarshalJSON(raw []byte) error {

	var aO0 Person
	if err := swag.ReadJSON(raw, &aO0); err != nil {
		return err
	}
	m.Person = aO0

	return nil
}

// MarshalJSON marshals this object to a JSON structure
func (m Nperson2) MarshalJSON() ([]byte, error) {
	var _parts [][]byte

	aO0, err := swag.WriteJSON(m.Person)
	if err != nil {
		return nil, err
	}
	_parts = append(_parts, aO0)

	return swag.ConcatJSON(_parts...), nil
}

// Validate validates this nperson2
func (m *Nperson2) Validate(formats strfmt.Registry) error {
	var res []error

	if err := m.Person.Validate(formats); err != nil {
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}
