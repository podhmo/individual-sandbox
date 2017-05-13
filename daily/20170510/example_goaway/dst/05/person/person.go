package person

import (
	"time"

	"github.com/podhmo/message"
)

// Person : ヒト
type Person struct {
	Name   string       `json:"name"`
	Age    int64        `json:"age"`
	Birth  time.Time    `json:"birth"`
	Father *Person      `json:"father"`
	Mother *Person      `json:"mother"`
	Info   message.Info `json:"info"`
	Info2  message.Info `json:"info2"`
}
