package main

// Conf :
type Conf struct {
	Bridge      Bridge      `json:"bridge" bson:"bridge"`
	Description string      `json:"description" bson:"description"`
	Platforms   Platforms   `json:"platforms" bson:"platforms"`
	Accessories Accessories `json:"accessories" bson:"accessories"`
}

// Bridge :
type Bridge struct {
	Port     int64  `json:"port" bson:"port"`
	Pin      string `json:"pin" bson:"pin"`
	Name     string `json:"name" bson:"name"`
	Username string `json:"username" bson:"username"`
}

// Platforms :
type Platforms []PlatformsItem

// PlatformsItem :
type PlatformsItem struct {
	Platform string `json:"platform" bson:"platform"`
	Name     string `json:"name" bson:"name"`
}

// Accessories :
type Accessories []AccessoriesItem

// AccessoriesItem :
type AccessoriesItem struct {
	Accessory string `json:"accessory" bson:"accessory"`
	Name      string `json:"name" bson:"name"`
}
