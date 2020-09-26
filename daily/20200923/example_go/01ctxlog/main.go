package main

import (
	"fmt"
	"sync"
)

type item struct {
	k interface{}
	v interface{}
}
type ChainMap struct {
	*sync.Map
	child *ChainMap
}

func (cm *ChainMap) Keys() []interface{} {
	return cm.keys(nil)
}

func (cm *ChainMap) keys(keys []interface{}) []interface{} {
	cm.Range(func(k interface{}, v interface{}) bool {
		for _, k0 := range keys {
			if k0 == k {
				return true
			}
		}
		keys = append(keys, k)
		return true
	})
	return keys
}

func (cm *ChainMap) Items() []item {
	return cm.items(nil)
}

func (cm *ChainMap) items(items []item) []item {
	cm.Range(func(k interface{}, v interface{}) bool {
		for _, p := range items {
			k0 := p.k
			if k0 == k {
				return true
			}
		}
		items = append(items, item{k: k, v: v})
		return true
	})
	return items
}

func (cm *ChainMap) Values() []interface{} {
	return cm.values(nil, nil)
}

func (cm *ChainMap) values(keys []interface{}, values []interface{}) []interface{} {
	cm.Range(func(k interface{}, v interface{}) bool {
		for _, k0 := range keys {
			if k0 == k {
				return true
			}
		}
		keys = append(keys, k)
		values = append(values, v)
		return true
	})
	return values
}

func (cm *ChainMap) Range(fn func(k, v interface{}) bool) {
	if cm.Map == nil {
		return
	}
	cm.Map.Range(fn)
	if cm.child == nil {
		return
	}
	cm.child.Range(fn)
}

func Chained(cm *ChainMap) *ChainMap {
	return &ChainMap{
		child: cm,
		Map:   &sync.Map{},
	}
}

func main() {
	c0 := Chained(nil)
	fmt.Println(c0.Items())
	c0.Store("x", 1)
	c0.Store("y", 2)
	fmt.Println("c0", c0.Items())

	c1 := Chained(c0)
	c1.Store("y", 20)
	c1.Store("z", 30)
	fmt.Println("c0", c0.Items(), "c1", c1.Items())

	c2 := Chained(c0)
	c2.Store("y", -20)
	c2.Store("x", -10)
	fmt.Println("c0", c0.Items(), "c1", c1.Items(), "c2", c2.Items())
}
