package main

// Bridge :
type Bridge struct {
	Name     string
	Username string
	Port     int
	Pin      string
}

// AccessoriesItem :
type AccessoriesItem struct {
	Accessory string
	Name      string
}

// Accessories :
type Accessories []AccessoriesItem

// PlatformsItem :
type PlatformsItem struct {
	Platform string
	Name     string
}

// Platforms :
type Platforms []PlatformsItem

// Conf :
type Conf struct {
	Bridge      Bridge
	Description string
	Accessories Accessories
	Platforms   Platforms
}
