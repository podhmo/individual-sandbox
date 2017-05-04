package main

// Conf :
type Conf struct {
	SetParameter      SetParameter      `json:"setParameter" bson:"setParameter"`
	Net               Net               `json:"net" bson:"net"`
	ProcessManagement ProcessManagement `json:"processManagement" bson:"processManagement"`
	SystemLog         SystemLog         `json:"systemLog" bson:"systemLog"`
	Storage           Storage           `json:"storage" bson:"storage"`
}

// SetParameter :
type SetParameter struct {
	EnableLocalhostAuthBypass bool `json:"enableLocalhostAuthBypass" bson:"enableLocalhostAuthBypass"`
}

// Net :
type Net struct {
	BindIP string `json:"bindIp" bson:"bindIp"`
	Port   int64  `json:"port" bson:"port"`
}

// ProcessManagement :
type ProcessManagement struct {
	Fork bool `json:"fork" bson:"fork"`
}

// SystemLog :
type SystemLog struct {
	LogAppend   bool   `json:"logAppend" bson:"logAppend"`
	Path        string `json:"path" bson:"path"`
	Destination string `json:"destination" bson:"destination"`
}

// Storage :
type Storage struct {
	Journal Journal `json:"journal" bson:"journal"`
}

// Journal :
type Journal struct {
	Enabled bool `json:"enabled" bson:"enabled"`
}
