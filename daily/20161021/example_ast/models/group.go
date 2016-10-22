package models

type Group struct {
	Name string `json:"name"`
}

func init() {
}

// archiveFormat is used to define the archive type when calling GetArchiveLink.
type archiveFormat string

const (
	// Tarball specifies an archive in gzipped tar format.
	Tarball archiveFormat = "tarball"

	// Zipball specifies an archive in zip format.
	Zipball archiveFormat = "zipball"
)

type N int

const (
	one N = 1
	two N = 2
)

// type rgb int

// const (
// 	r rgb = iota
// 	g
// 	b
// )
