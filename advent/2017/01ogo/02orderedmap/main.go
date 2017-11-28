package main

import (
	"fmt"
	"strconv"
	"sync"
)

// Omap :
type Omap struct {
	m    map[int]int
	keys []int
}

// New :
func New() *Omap {
	return &Omap{
		m:    map[int]int{},
		keys: []int{},
	}
}

func (m *Omap) set(k int, v int) {
	if _, ok := m.m[k]; !ok {
		m.keys = append(m.keys, k)
	}
	m.m[k] = v
}

func (m *Omap) get(k int) int {
	return m.m[k]
}

func (m *Omap) iterate(fn func(int, int)) {
	for _, k := range m.keys {
		fn(k, m.m[k])
	}
}

func main() {
	m := New()
	for i := 0; i < 101; i++ {
		for _, c := range strconv.Itoa(i) {
			n := int(c) - '0'
			m.set(n, m.get(n)+1)
		}
	}

	fmt.Println("before")
	m.iterate(func(k int, v int) {
		fmt.Println(k, v)
	})
	fmt.Println("after")

	{
		m := New()
		fmt.Println("before")
		var lock sync.Mutex
		var wg sync.WaitGroup
        wg.Add(1)
		go func() {
			for i := 0; i < 1000; i++ {
				lock.Lock()
				m.set(1, m.get(1)+1)
				lock.Unlock()
			}
			wg.Done()
		}()
        wg.Add(1)
		go func() {
			for i := 0; i < 1000; i++ {
				lock.Lock()
				m.set(1, m.get(1)+1)
				lock.Unlock()
			}
			wg.Done()
		}()
        wg.Add(1)
		go func() {
			for i := 0; i < 1000; i++ {
				lock.Lock()
				m.set(1, m.get(1)+1)
				lock.Unlock()
			}
			wg.Done()
		}()

		wg.Wait()
		m.iterate(func(k int, v int) {
			fmt.Println(k, v)
		})
		fmt.Println("after")
	}
}
