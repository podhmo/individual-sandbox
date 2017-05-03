package main

// Conf :
type Conf struct {
	Bridge      *Bridge     `json:"bridge" bson:"bridge"`
	Description string      `json:"description" bson:"description"`
	Platforms   Platforms   `json:"platforms" bson:"platforms"`
	Accessories Accessories `json:"accessories" bson:"accessories"`
}

// Bridge :
type Bridge struct {
	Name     string `json:"name" bson:"name"`
	Port     int64  `json:"port" bson:"port"`
	Username string `json:"username" bson:"username"`
	Pin      string `json:"pin" bson:"pin"`
}

// Platforms :
type Platforms []PlatformsItem

// PlatformsItem :
type PlatformsItem struct {
	Name     string `json:"name" bson:"name"`
	Platform string `json:"platform" bson:"platform"`
}

// Accessories :
type Accessories []AccessoriesItem

// AccessoriesItem :
type AccessoriesItem struct {
	Name      string `json:"name" bson:"name"`
	Accessory string `json:"accessory" bson:"accessory"`
}
