package add

import (
	"encoding/json"
	"fmt"
	"net/http"
)

type Input struct {
	Values []int
}

func Add(w http.ResponseWriter, req *http.Request) {
	var data Input
	decoder := json.NewDecoder(req.Body)
	if err := decoder.Decode(&data); err != nil {
		w.WriteHeader(400)
		fmt.Fprintf(w, `{"error": %q}`, err.Error())
		return
	}

	var n int
	for _, v := range data.Values {
		n += v
	}
	w.Header().Set("Content-Type", "application/json")
	encoder := json.NewEncoder(w)
	if err := encoder.Encode(map[string]int{"result": n}); err != nil {
		panic(err)
	}
}
