package clientgen

import (
	"encoding/json"
	"fmt"
	"net/http"
)

type Decoder struct{}

func NewDecoder() *Decoder {
	return &Decoder{}
}

func (t *Decoder) DecodeError(resp *http.Response) error {
	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("error: %s", resp.Status)
	}
	return nil
}
func (t *Decoder) DecodeResult(resp *http.Response, result interface{}) error {
	decoder := json.NewDecoder(resp.Body)
	if err := decoder.Decode(result); err != nil {
		return fmt.Errorf("decode json: %w", err)
	}
	return nil
}
