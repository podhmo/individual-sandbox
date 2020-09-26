package main

import (
	"encoding/json"
	"os"
)

func main() {
	data := map[string]string{"message": "<<hello>>"}
	{
		encoder := json.NewEncoder(os.Stdout)
		encoder.Encode(data)
	}
	{
		encoder := json.NewEncoder(os.Stdout)
		encoder.SetEscapeHTML(true)
		encoder.Encode(data)
	}
	{
		encoder := json.NewEncoder(os.Stdout)
		encoder.SetEscapeHTML(false)
		encoder.Encode(data)
	}
}
