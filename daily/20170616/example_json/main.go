package main

// Slave : 
type Slave []SlaveItem


// SlaveItem : 
type SlaveItem struct {
	Weight int64 `json:"weight"`
	IP string `json:"ip"`
}


// Server : 
type Server struct {
	Host string `json:"host"`
	Port string `json:"port"`
	Slave Slave `json:"slave"`
}


// Db : 
type Db struct {
	User string `json:"user"`
	Pass string `json:"pass"`
}


// Config : 
type Config struct {
	Server Server `json:"server"`
	Db Db `json:"db"`
}