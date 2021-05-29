package main

import (
	"encoding/json"
	"fmt"
	"log"
	"os"
	"strings"
	"time"
)

// 型付きID
// あんまり速度のことは考えていない状態

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	var ob Board
	s := `
{
  "id": "tb_1",
  "lanes": [
    {
      "id": "tl_10",
      "board_id": "tb_1",
      "name": "todo",
      "items": [
        {"title": "go to bed", "lane_id": "tl_10", "id": "ti_1234", "parent_id": "ti_1"}
      ]
    }
  ]
}
`

	fmt.Println(">>>", s)
	if err := json.Unmarshal([]byte(s), &ob); err != nil {
		return err
	}
	fmt.Printf("--- %#+v\n", ob)

	fmt.Print("<<< ")
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	return enc.Encode(ob)
}

const ITEM_PREFIX = "ti_"

type ItemID string

func (id *ItemID) UnmarshalText(b []byte) error {
	s := string(b)
	if s == "null" {
		return nil
	}

	if len(b) <= len(ITEM_PREFIX) || !strings.HasPrefix(s, ITEM_PREFIX) {
		return nil
	}
	*id = ItemID(s[len(ITEM_PREFIX):])
	return nil
}

func (id ItemID) MarshalText() ([]byte, error) {
	if id == "" {
		return nil, nil
	}
	return []byte(ITEM_PREFIX + id), nil
}

const BOARD_PREFIX = "tb_"

type BoardID string

func (id *BoardID) UnmarshalText(b []byte) error {
	s := string(b)
	if s == "null" {
		return nil
	}

	if len(b) <= len(BOARD_PREFIX) || !strings.HasPrefix(s, BOARD_PREFIX) {
		return nil
	}
	*id = BoardID(s[len(BOARD_PREFIX):])
	return nil
}

func (id BoardID) MarshalText() ([]byte, error) {
	if id == "" {
		return nil, nil
	}
	return []byte(BOARD_PREFIX + id), nil
}

const LANE_PREFIX = "tl_"

type LaneID string

func (id *LaneID) UnmarshalText(b []byte) error {
	s := string(b)
	if s == "null" {
		return nil
	}

	if len(b) <= len(LANE_PREFIX) || !strings.HasPrefix(s, LANE_PREFIX) {
		return nil
	}
	*id = LaneID(s[len(LANE_PREFIX):])
	return nil
}

func (id LaneID) MarshalText() ([]byte, error) {
	if id == "" {
		return nil, nil
	}
	return []byte(LANE_PREFIX + id), nil
}

type Board struct {
	ID BoardID `json:"id"`

	Name  string `json:"name"`
	Lanes []Lane `json:"lanes"`
}
type Lane struct {
	ID      LaneID  `json:"id"`
	BoardID BoardID `json:"board_id"`

	Name  string `json:"name"`
	Items []Item `json:"items"`
}
type Item struct {
	ID     ItemID `json:"id"`
	LaneID LaneID `json:"lane_id"`

	Title      string     `json:"title"`
	FinishedAt *time.Time `json:"finished_at"`

	ParentID *ItemID `json:"parent_id"`
}
