package model

type Page struct {
	Path     string `bson:"path" json:"path"`
	PathHash string `bson:"pathHash" json:"pathHash"`
	Title    string `bson:"title" json:"title"`
}
