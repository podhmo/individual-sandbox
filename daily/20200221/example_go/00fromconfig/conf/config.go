package conf

// Config ...
type Config struct {
	DB         DBConfig         `json:"db"`
	Thirdparty ThirdpartyConfig `json:"thirdparty"`
}

// DBConfig ...
type DBConfig struct {
	URL string `json:"url"`
}

// ThirdpartyConfig ...
type ThirdpartyConfig struct {
	XXX XXXConfig `json:"xxx"`
	YYY YYYConfig `json:"yyy"`
	ZZZ ZZZConfig `json:"zzz"`
}

// XXXConfig ...

