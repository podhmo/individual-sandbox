package main

// Conf :my config file
type Conf struct {
	ProcessManagement ProcessSetting `json:"processManagement" bson:"processManagement"`
	SetParameter      SetParameter   `json:"setParameter" bson:"setParameter"`
	SystemLog         LogSetting     `json:"systemLog" bson:"systemLog"`
	Storage           StorageSetting `json:"storage" bson:"storage"`
	Net               NetSetting     `json:"net" bson:"net"`
}

// ProcessSetting :
type ProcessSetting struct {
	Fork bool `json:"fork" bson:"fork"`
}

// SetParameter :
type SetParameter struct {
	EnableLocalhostAuthBypass bool `json:"enableLocalhostAuthBypass" bson:"enableLocalhostAuthBypass"`
}

// LogSetting :
type LogSetting struct {
	LogAppend   bool   `json:"logAppend" bson:"logAppend"`
	Destination string `json:"destination" bson:"destination"`
	Path        string `json:"path" bson:"path"`
}

// StorageSetting :
type StorageSetting struct {
	Journal Journal `json:"journal" bson:"journal"`
}

// Journal :
type Journal struct {
	Enabled bool `json:"enabled" bson:"enabled"`
}

// NetSetting :
type NetSetting struct {
	BindIP string `json:"bindIp" bson:"bindIp"`
	Port   int64  `json:"port" bson:"port"`
}
