package main

// Config :
type Config struct {
	Appconf Appconf `json:"appconf"`
}

// Appconf :
type Appconf struct {
	Endpoint string `json:"endpoint"`
	Key      string `json:"key"`
	Secret   string `json:"secret"`
}
