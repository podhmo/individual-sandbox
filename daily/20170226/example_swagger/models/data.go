package models

// This file was generated by the swagger tool.
// Editing this file might prove futile when you re-run the swagger generate command

import (
	strfmt "github.com/go-openapi/strfmt"

	"github.com/go-openapi/errors"
	"github.com/go-openapi/validate"
)

// Data data
// swagger:model data
type Data struct {

	// access token
	// Required: true
	// Max Length: 64
	AccessToken *string `json:"access_token"`

	// account id
	// Required: true
	AccountID *int64 `json:"account_id"`
}

// Validate validates this data
func (m *Data) Validate(formats strfmt.Registry) error {
	var res []error

	if err := m.validateAccessToken(formats); err != nil {
		// prop
		res = append(res, err)
	}

	if err := m.validateAccountID(formats); err != nil {
		// prop
		res = append(res, err)
	}

	if len(res) > 0 {
		return errors.CompositeValidationError(res...)
	}
	return nil
}

func (m *Data) validateAccessToken(formats strfmt.Registry) error {

	if err := validate.Required("access_token", "body", m.AccessToken); err != nil {
		return err
	}

	if err := validate.MaxLength("access_token", "body", string(*m.AccessToken), 64); err != nil {
		return err
	}

	return nil
}

func (m *Data) validateAccountID(formats strfmt.Registry) error {

	if err := validate.Required("account_id", "body", m.AccountID); err != nil {
		return err
	}

	return nil
}
