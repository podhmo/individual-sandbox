package conf

// https://docs.docker.com/compose/compose-file/
// https://mholt.github.io/json-to-go/

type Config struct {
	Version  string   `json:"version"`
	Services Services `json:"services"`
	Volumes  Volumes  `json:"volumes"`
	Networks Networks `json:"networks"`
}
type Deploy struct {
	Mode         string `json:"mode"`
	Replicas     int    `json:"replicas"`
	EndpointMode string `json:"endpoint_mode"`
}
type Wordpress struct {
	Image    string   `json:"image"`
	Ports    []string `json:"ports"`
	Networks []string `json:"networks"`
	Deploy   Deploy   `json:"deploy"`
}
type Mysql struct {
	Image    string   `json:"image"`
	Volumes  []string `json:"volumes"`
	Networks []string `json:"networks"`
	Deploy   Deploy   `json:"deploy"`
}
type Services struct {
	Wordpress Wordpress `json:"wordpress"`
	Mysql     Mysql     `json:"mysql"`
}
type Volumes struct {
	DbData interface{} `json:"db-data"`
}
type Networks struct {
	Overlay interface{} `json:"overlay"`
}
