@ {}
@ {'inline': False, 'required': False, 'comment': ''}
package main

import (
	"github.com/podhmo/errmap"
	"encoding/json"
)

func (p *Person) UnmarshalJSON(b []byte) error {
	var err *errmap.Error

	// loading internal data
	var inner struct {
		Name *string `json:"name"`
		Nickname *string `json:"nickname"`
	}
	if rawErr := json.Unmarshal(b, &inner); rawErr != nil  {
		return err.addSummary(rawErr.Error())
	}

	// binding field value and required check
	if Name != nil  {
		p.Name = *Name
	} else  {
		err = err.Add("name", "required")
	}
	if Nickname != nil  {
		p.Nickname = *Nickname
	}

	return err.Untyped()
}
