## notion

- download

### 追記

structgenを作りたくなった。

### 追記

なんか無限再帰しちゃうな。。

```
path ::
0  struct.Page
1  struct.BlockRecords slice[0]
2  struct.Activity
3  struct.Edits slice[0]
4  struct.BlockData
5  struct.BlockValue
6  struct.TableViews slice[0]
7  struct.CollectionView
8  struct.Format
9  struct.TableProperties slice[0] *]
```

```go
// Page describes a single Notion page
type Page struct {
	ID string

	// expose raw records for all data associated with this page
	BlockRecords          []*Record // VISIT: 1



	UserRecords           []*Record
	CollectionRecords     []*Record
	CollectionViewRecords []*Record
	DiscussionRecords     []*Record
	CommentRecords        []*Record

	// for every block of type collection_view and its view_ids
	// we } TableView representing that collection view_id
	TableViews []*TableView

	idToBlock          map[string]*Block
	idToUser           map[string]*User
	idToCollection     map[string]*Collection
	idToCollectionView map[string]*CollectionView
	idToComment        map[string]*Comment
	idToDiscussion     map[string]*Discussion

	blocksToSkip map[string]struct{} // not alive or when server doesn't return "value" for this block id

	client *Client
}

// Record represents a polymorphic record
type Record struct {
	// fields returned by the server
	Role string `json:"role"`
	// polymorphic value of the record, which we decode into Block, Space etc.
	Value json.RawMessage `json:"value"`

	// fields set from Value based on type
	ID    string `json:"-"`
	Table string `json:"-"`

	Activity       *Activity       `json:"-"` // VISIT: 2
	Block          *Block          `json:"-"`
	Space          *Space          `json:"-"`
	User           *User           `json:"-"`
	Collection     *Collection     `json:"-"`
	CollectionView *CollectionView `json:"-"`
	Comment        *Comment        `json:"-"`
	Discussion     *Discussion     `json:"-"`
	// TODO: add more types
}

// Activity represents a Notion activity (ie. event)
type Activity struct {
	Role string `json:"role"`

	ID        string `json:"id"`
	SpaceID   string `json:"space_id"`
	StartTime string `json:"start_time"`
	EndTime   string `json:"end_time"`
	Type      string `json:"type"`
	Version   int    `json:"version"`

	ParentID    string `json:"parent_id"`
	ParentTable string `json:"parent_table"`

	// If the edit was to a block inside a regular page
	NavigableBlockID string `json:"navigable_block_id"`

	// If the edit was to a block inside a collection or collection row
	CollectionID    string `json:"collection_id"`
	CollectionRowID string `json:"collection_row_id"`

	Edits []Edit `json:"edits"` // VISIT: 3

	Index   int  `json:"index"`
	Invalid bool `json:"invalid"`

	RawJSON map[string]interface{} `json:"-"`
}


// Edit represents a Notion edit (ie. a change made during an Activity)
type Edit struct {
	SpaceID   string   `json:"space_id"`
	Authors   []Author `json:"authors"`
	Timestamp int64    `json:"timestamp"`
	Type      string   `json:"type"`
	Version   int      `json:"version"`

	CommentData  Comment `json:"comment_data"`
	CommentID    string  `json:"comment_id"`
	DiscussionID string  `json:"discussion_id"`

	BlockID   string `json:"block_id"`
	BlockData struct {
		BlockValue Block `json:"block_value"` // VISIT: 5
	} `json:"block_data"` // VISIT: 4
	NavigableBlockID string `json:"navigable_block_id"`

	CollectionID    string `json:"collection_id"`
	CollectionRowID string `json:"collection_row_id"`
}

// Block describes a block
type Block struct {
	// values that come from JSON
	// a unique ID of the block
	ID string `json:"id"`
	// if false, the page is deleted
	Alive bool `json:"alive"`
	// List of block ids for that make up content of this block
	// Use Content to get corresponding block (they are in the same order)
	ContentIDs   []string `json:"content,omitempty"`
	CopiedFrom   string   `json:"copied_from,omitempty"`
	CollectionID string   `json:"collection_id,omitempty"` // for BlockCollectionView
	// ID of the user who created this block
	CreatedBy   string `json:"created_by"`
	CreatedTime int64  `json:"created_time"`

	CreatedByTable    string `json:"created_by_table"`     // e.g. "notion_user"
	CreatedByID       string `json:"created_by_id"`        // e.g. "bb760e2d-d679-4b64-b2a9-03005b21870a",
	LastEditedByTable string `json:"last_edited_by_table"` // e.g. "notion_user"
	LastEditedByID    string `json:"last_edited_by_id"`    // e.g. "bb760e2d-d679-4b64-b2a9-03005b21870a"

	// List of block ids with discussion content
	DiscussionIDs []string `json:"discussion,omitempty"`
	// those ids seem to map to storage in s3
	// https://s3-us-west-2.amazonaws.com/secure.notion-static.com/${id}/${name}
	FileIDs []string `json:"file_ids,omitempty"`

	// TODO: don't know what this means
	IgnoreBlockCount bool `json:"ignore_block_count,omitempty"`

	// ID of the user who last edited this block
	LastEditedBy   string `json:"last_edited_by"`
	LastEditedTime int64  `json:"last_edited_time"`
	// ID of parent Block
	ParentID    string `json:"parent_id"`
	ParentTable string `json:"parent_table"`
	// not always available
	Permissions *[]Permission          `json:"permissions,omitempty"`
	Properties  map[string]interface{} `json:"properties,omitempty"`
	// type of the block e.g. TypeText, TypePage etc.
	Type string `json:"type"`
	// blocks are versioned
	Version int64 `json:"version"`
	// for BlockCollectionView
	ViewIDs []string `json:"view_ids,omitempty"`

	// Parent of this block
	Parent *Block `json:"-"`

	// maps ContentIDs array to Block type
	Content []*Block `json:"-"`
	// this is for some types like TypePage, TypeText, TypeHeader etc.
	InlineContent []*TextSpan `json:"-"`

	// for BlockPage
	Title string `json:"-"`

	// For BlockTodo, a checked state
	IsChecked bool `json:"-"`

	// for BlockBookmark
	Description string `json:"-"`
	Link        string `json:"-"`

	// for BlockBookmark it's the url of the page
	// for BlockGist it's the url for the gist
	// fot BlockImage it's url of the image, but use ImageURL instead
	// because Source is sometimes not accessible
	// for BlockFile it's url of the file
	// for BlockEmbed it's url of the embed
	Source string `json:"-"`

	// for BlockFile
	FileSize string `json:"-"`

	// for BlockImage it's an URL built from Source that is always accessible
	ImageURL string `json:"-"`

	// for BlockCode
	Code         string `json:"-"`
	CodeLanguage string `json:"-"`

	// for BlockCollectionView. There can be multiple views
	// those correspond to ViewIDs
	TableViews []*TableView `json:"-"` // VISIT: 6

	Page *Page `json:"-"`

	// RawJSON represents Block as
	RawJSON map[string]interface{} `json:"-"`

	isResolved bool
}

// TableView represents a view of a table (Notion calls it a Collection View)
// Meant to be a representation that is easier to work with
type TableView struct {
	// original data
	Page           *Page
	CollectionView *CollectionView // VISIT: 7
	Collection     *Collection

	// easier to work representation we calculate
	Columns []*ColumnInfo
	Rows    []*TableRow
}

// CollectionView represents a collection view
type CollectionView struct {
	ID          string       `json:"id"`
	Version     int64        `json:"version"`
	Type        string       `json:"type"` // "table"
	Format      *FormatTable `json:"format"` // VISIT: 8
	Name        string       `json:"name"`
	ParentID    string       `json:"parent_id"`
	ParentTable string       `json:"parent_table"`
	Query       *Query       `json:"query"`
	Query2      *Query2      `json:"query2"`
	Alive       bool         `json:"alive"`
	PageSort    []string     `json:"page_sort"`

	// set by us
	RawJSON map[string]interface{} `json:"-"`
}

// FormatTable describes format for BlockTable
type FormatTable struct {
	PageSort        []string         `json:"page_sort"`
	TableWrap       bool             `json:"table_wrap"`
	TableProperties []*TableProperty `json:"table_properties"` // VISIT: 9
}
```

