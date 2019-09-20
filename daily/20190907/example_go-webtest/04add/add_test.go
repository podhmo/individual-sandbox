package add

import (
	"bytes"
	"net/http"
	"testing"

	webtest "github.com/podhmo/go-webtest"
	"github.com/podhmo/go-webtest/try"
)

func TestHandler(t *testing.T) {
	c := webtest.NewClientFromHandler(http.HandlerFunc(Add))
	var right interface{}
	try.It{
		Code: 200,
		Want: &right,
	}.With(t, c,
		"POST", "/",
		webtest.WithJSON(bytes.NewBufferString(`{"values": [1,2,4]}`)),
	)
}
