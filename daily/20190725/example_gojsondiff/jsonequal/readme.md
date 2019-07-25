# jsonequal

```go
func Test(t *testing.T) {
	v := map[string]int{"foo": 1}
	b := []bytes(`{"foo": 1}`)
	r := bytes.NewBufferString(`{"foo": 1}`)

	if err := jsonequal.Equal(jsonequal.From(v), jsonequal.FromBytes(b)); err != nil {
		t.Errorf("mismatch: %s", err)
	}

	if err := jsonequal.Equal(jsonequal.From(v), jsonequal.FromReader(r)); err != nil {
		t.Errorf("mismatch: %s", err)
	}
}
```
