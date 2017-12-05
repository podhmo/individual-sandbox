package main

import (
	"fmt"
	"strconv"
)

type link struct {
	prev *link
	next *link
	key  int
}

// Omap :
type Omap struct {
	m     map[int]int
	start *link
	end   *link
	zero  int
}

// New :
func New() *Omap {
	return &Omap{
		m: map[int]int{},
	}
}

func (m *Omap) set(k int, v int) {
	if _, ok := m.m[k]; !ok {
		if m.start == nil {
			l := &link{key: k}
			m.start = l
			m.end = l
		} else {
			l := &link{key: k, prev: m.end}
			m.end.next = l
			m.end = l
		}
	}
	m.m[k] = v
}

func (m *Omap) get(k int) int {
	return m.m[k]
}

func (m *Omap) unset(k int) {
	if v, ok := m.m[k]; ok {
		fmt.Println("#", "unset", k, v)
		// unlink (O(N))
		for l := m.start; l != nil; l = l.next {
			if l.key == k {
				if m.start == l && m.end == l {
					m.start = nil
					m.end = nil
				} else if l.next == nil {
					l.prev.next = nil
					m.end = l.prev
				} else if l.prev == nil {
					m.start = l.next
				} else {
					prev := l.prev
					l.prev.next = l.next
					l.next.prev = prev
				}
				break
			}
		}
		delete(m.m, k)
	}
}

func (m *Omap) iterate(fn func(int, int)) {
	for l := m.start; l != nil; l = l.next {
		fn(l.key, m.m[l.key])
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

		// ordmap := map[int]int{}
		// for i := 9; i >= 0; i-- {
		// 	ordmap[i] = i
		// }
		// for k := range ordmap {
		// 	m.unset(k)
		// }

		for i := 9; i >= 0; i-- {
			m.unset(i)
		}

		// for i := 0; i < 10; i++ {
		// 	m.unset(i)
		// }
		fmt.Println("before")
		m.iterate(func(k int, v int) {
			fmt.Println("@@", k, v)
		})
		fmt.Println("after")
	}
}
