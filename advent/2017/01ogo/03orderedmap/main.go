package main

import (
	"fmt"
	"strconv"
	"sync"
)

type link struct {
	prev  *link
	next  *link
	key   int
	value int
}

// Omap :
type Omap struct {
	m     map[int]*link
	start *link
	end   *link
	zero  int
}

// New :
func New() *Omap {
	root := &link{}
	return &Omap{
		m:     map[int]*link{},
		start: root,
		end:   root,
	}
}

func (m *Omap) set(k int, v int) {
	l, ok := m.m[k]
	if !ok {
		l = &link{value: v, key: k, prev: m.end}
		m.m[k] = l
		m.end.next = l
		m.end = l
	} else {
		l.value = v
	}
}

func (m *Omap) get(k int) int {
	if l, ok := m.m[k]; ok {
		return l.value
	}
	return m.zero
}

func (m *Omap) unset(k int) {
	if l, ok := m.m[k]; ok {
		fmt.Println("#", "unset", l.key, l.value)
		if l.next == nil {
			l.prev = nil
		} else {
			prev := l.prev
			l.prev.next = l.next
			l.next.prev = prev
		}
		delete(m.m, k)
	}
}

func (m *Omap) iterate(fn func(int, int)) {
	l := m.start
	if l.next == nil {
		return
	}
	for l := l.next; l != nil; l = l.next {
		fn(l.key, l.value)
	}
}

func main() {
	m := New()
	for i := 0; i < 101; i++ {
		for _, c := range strconv.Itoa(i) {
			n := int(c) - '0'
			k := m.get(n)
			m.set(n, k+1)
		}
	}

	fmt.Println("before")
	m.iterate(func(k int, v int) {
		fmt.Println(k, v)
	})
	fmt.Println("after")

	{
		m.unset(3)
		m.unset(5)
		m.unset(7)
		m.unset(9)
		fmt.Println("before")
		m.iterate(func(k int, v int) {
			fmt.Println("@", k, v)
		})
		fmt.Println("after")
		for i := 0; i < 10; i++ {
			m.unset(i)
		}
		fmt.Println("before")
		m.iterate(func(k int, v int) {
			fmt.Println("@@", k, v)
		})
		fmt.Println("after")
	}

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
			fmt.Println("@", k, v)
		})
		fmt.Println("after")
	}
}
