package model


type Page struct {
	Id       string `bson:"_id" json:"id"`
	Path     string        `bson:"path" json:"path"`
	PathHash string        `bson:"pathHash" json:"pathHash"`
	Title    string        `bson:"title" json:"title"`
}
