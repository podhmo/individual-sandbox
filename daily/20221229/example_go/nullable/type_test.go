package nullable_test

import (
	"bytes"
	"encoding/json"
	"errors"
	"strings"
	"testing"

	"github.com/podhmo/individual-sandbox/daily/20221229/example_go/nullable"
)

func jsonNormalize(t *testing.T, val []byte) []byte {
	buf := new(bytes.Buffer)
	if err := json.Compact(buf, val); err != nil {
		t.Fatalf("unexpected json: %v", err)
	}
	return buf.Bytes()
}

func TestJSON(t *testing.T) {
	decodeJSON := func(input string, ob any) (any, error) {
		if err := json.NewDecoder(strings.NewReader(input)).Decode(ob); err != nil {
			return nil, err
		}
		return ob, nil
	}
	cases := []struct {
		msg    string
		input  string
		want   string
		decode func(input string) (any, error)

		assertDecodeError func(*testing.T, error)
	}{
		{
			msg:   "ok-string",
			input: `{"string": "foo", "zero_string": "", "null_string": null}`,
			want:  `{"string": "foo", "zero_string": "", "null_string": null, "omitempty_string": null}`,
			decode: func(input string) (any, error) {
				var ob struct {
					String          nullable.Type[string] `json:"string"`
					ZeroString      nullable.Type[string] `json:"zero_string"`
					NullString      nullable.Type[string] `json:"null_string"`
					OmitemptyString nullable.Type[string] `json:"omitempty_string,omitempty"`
				}
				return decodeJSON(input, &ob)
			},
		},
		{
			msg:   "ok-int32",
			input: `{"int32": 100, "zero_int32": 0, "null_int32": null}`,
			want:  `{"int32": 100, "zero_int32": 0, "null_int32": null, "omitempty_int32": null}`,
			decode: func(input string) (any, error) {
				var ob struct {
					Int32          nullable.Type[int32] `json:"int32"`
					Zeroint32      nullable.Type[int32] `json:"zero_int32"`
					Nullint32      nullable.Type[int32] `json:"null_int32"`
					Omitemptyint32 nullable.Type[int32] `json:"omitempty_int32,omitempty"`
				}
				return decodeJSON(input, &ob)
			},
		},
		{
			msg:   "ng-int32",
			input: `{"int32": "foo"}`,
			decode: func(input string) (any, error) {
				var ob struct {
					Int32 nullable.Type[int32] `json:"int32"`
				}
				return decodeJSON(input, &ob)
			},
			assertDecodeError: func(t *testing.T, err error) {
				want := &json.UnmarshalTypeError{}
				if !errors.As(err, &want) {
					t.Errorf("mismatch decode error: %T != %T", err, want)
				}
			},
		},
		{
			msg:   "ok-newType",
			input: `{"ordering": "desc", "zero_ordering": "", "null_ordering": null}`,
			want:  `{"ordering": "desc", "zero_ordering": "", "null_ordering": null, "omitempty_ordering": null}`,
			decode: func(input string) (any, error) {
				type Ordering string
				var ob struct {
					Ordering          nullable.Type[Ordering] `json:"ordering"`
					ZeroOrdering      nullable.Type[Ordering] `json:"zero_ordering"`
					NullOrdering      nullable.Type[Ordering] `json:"null_ordering"`
					OmitemptyOrdering nullable.Type[Ordering] `json:"omitempty_ordering,omitempty"`
				}
				return decodeJSON(input, &ob)
			},
		},
	}

	for _, c := range cases {
		c := c
		t.Run(c.msg, func(t *testing.T) {
			buf := new(bytes.Buffer)
			t.Logf(" input json: %s", c.input)

			ob, err := c.decode(c.input)
			if c.assertDecodeError != nil {
				if err == nil {
					t.Fatalf("must be error")
				}
				c.assertDecodeError(t, err)
				return
			}

			if err != nil {
				t.Fatalf("unexpected decode error: %+v", err)
			}
			if err := json.NewEncoder(buf).Encode(ob); err != nil {
				t.Errorf("unexpected encode error: %+v", err)
			}

			t.Logf("output json: %s", buf.String())
			if want, got := jsonNormalize(t, []byte(c.want)), jsonNormalize(t, buf.Bytes()); !bytes.Equal(want, got) {
				t.Errorf("mismatch json\nwant:\n\t%q\ngot:\n\t%q", string(want), string(got))
			}
		})
	}
}
