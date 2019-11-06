package schema

import (
	"database/sql"
)

const (
	FOREIGN_KEY = "FOREIGN KEY"
)

// Index is the struct for database index
type Index struct {
	Name    string   `json:"name"`
	Def     string   `json:"def"`
	Table   *string  `json:"table"`
	Columns []string `json:"columns"`
}

// Constraint is the struct for database constraint
type Constraint struct {
	Name             string   `json:"name"`
	Type             string   `json:"type"`
	Def              string   `json:"def"`
	Table            *string  `json:"table"`
	ReferenceTable   *string  `json:"reference_table"`
	Columns          []string `json:"columns"`
	ReferenceColumns []string `json:"reference_columns"`
}

// Trigger is the struct for database trigger
type Trigger struct {
	Name string `json:"name"`
	Def  string `json:"def"`
}

// Column is the struct for table column
type Column struct {
	Name            string         `json:"name"`
	Type            string         `json:"type"`
	Nullable        bool           `json:"nullable"`
	Default         sql.NullString `json:"default"`
	Comment         string         `json:"comment"`
	ParentRelations []*Relation    `json:"-"`
	ChildRelations  []*Relation    `json:"-"`
}

// Table is the struct for database table
type Table struct {
	Name        string        `json:"name"`
	Type        string        `json:"type"`
	Comment     string        `json:"comment"`
	Columns     []*Column     `json:"columns"`
	Indexes     []*Index      `json:"indexes"`
	Constraints []*Constraint `json:"constraints"`
	Triggers    []*Trigger    `json:"triggers"`
	Def         string        `json:"def"`
}

// Relation is the struct for table relation
type Relation struct {
	Table         *Table    `json:"table"`
	Columns       []*Column `json:"columns"`
	ParentTable   *Table    `json:"parent_table"`
	ParentColumns []*Column `json:"parent_columns"`
	Def           string    `json:"def"`
	IsAdditional  bool      `json:"is_additional"`
}

// Driver is the struct for tbls driver information
type Driver struct {
	Name            string `json:"name"`
	DatabaseVersion string `json:"database_version"`
}

// Schema is the struct for database schema
type Schema struct {
	Name      string      `json:"name"`
	Tables    []*Table    `json:"tables"`
	Relations []*Relation `json:"relations"`
	Driver    *Driver     `json:"driver"`
}
