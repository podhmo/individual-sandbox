package info

import (
	"net/http"
	"reflect"
)

// reflection

type Info map[string]interface{}

func InfoFromInterface(resp interface{}, excludes []string) Info {
	rt := reflect.TypeOf(resp).Elem()
	rv := reflect.ValueOf(resp).Elem()
	info := Info{}

toplevel:
	for i := 0; i < rt.NumField(); i++ {
		rf := rt.Field(i)
		for _, name := range excludes {
			if name == rf.Name {
				continue toplevel
			}
		}
		info[rf.Name] = rv.Field(i).Interface()
	}
	return info
}

func InfoFromRequest(req *http.Request) Info {
	return InfoFromInterface(req, []string{
		"URL", "Body", "GetBody", "Close", "Trailer", "TLS", "Cancel", "Response", "ctx",
	})
}

func InfoFromResponse(resp *http.Response) Info {
	info := InfoFromInterface(resp, []string{
		"Close", "Body", "Trailer", "Request", "TLS", "Request",
	})
	return info
}
