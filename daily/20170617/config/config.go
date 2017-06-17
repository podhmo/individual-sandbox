package main

import (
	"/net/url"
)

// Config :
type Config struct {
	Appconf Appconf `json:"appconf"`
}

// Appconf :
type Appconf struct {
	Endpoint url.URL `json:"endpoint"`
	Key      string  `json:"key"`
	Secret   string  `json:"secret"`
}
