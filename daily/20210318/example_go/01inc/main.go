package main

import (
	"encoding/json"
	"os"
	"sync"
	"sync/atomic"
	"time"
)

// ConnectionCounter :
type ConnectionCounter struct {
	mu   sync.Mutex
	once sync.Once

	Data ConnectionData
}

// RoundTrip : get or create cache
func (c *ConnectionCounter) Inc(d int64) {
	c.once.Do(func() {
		go func() {
			for {
				con := atomic.LoadInt64(&c.Data.Connection)
				c.mu.Lock()
				if c.Data.MaxConnection < con {
					c.Data.MaxConnection = con
				}
				c.mu.Unlock()

				<-time.After(1 * time.Second)
			}
		}()
	})
	atomic.AddInt64(&c.Data.Connection, d)
}

func (c *ConnectionCounter) ExtractInfo() ConnectionData {
	return c.Data
}

// ConnectionData :
type ConnectionData struct {
	Connection    int64 `json:"connection"`
	MaxConnection int64 `json:"maxConnection"`
}

func main() {
	c := &ConnectionCounter{}
	c.Inc(1)
	c.Inc(10)
	json.NewEncoder(os.Stdout).Encode(c.ExtractInfo())
	c.Inc(-10)
	json.NewEncoder(os.Stdout).Encode(c.ExtractInfo())
}
