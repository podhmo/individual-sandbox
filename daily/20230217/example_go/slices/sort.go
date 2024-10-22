package slices

import "sort"

// from: https://pkg.go.dev/golang.org/x/exp/constraints
type Signed interface {
	~int | ~int8 | ~int16 | ~int32 | ~int64
}
type Unsigned interface {
	~uint | ~uint8 | ~uint16 | ~uint32 | ~uint64 | ~uintptr
}
type Integer interface {
	Signed | Unsigned
}
type Float interface {
	~float32 | ~float64
}
type Ordered interface {
	Integer | Float | ~string
}

func SortBy[K Ordered, T any](xs []T, fn func(T) K) []T {
	m := make(map[K][]int, len(xs))
	keys := make([]K, 0, len(xs))
	for i, x := range xs {
		k := fn(x)
		v, ok := m[k]
		if !ok {
			keys = append(keys, k)
		}
		m[k] = append(v, i)

	}
	sort.SliceStable(keys, func(i int, j int) bool { return keys[i] < keys[j] })
	r := make([]T, 0, len(xs))
	for _, k := range keys {
		for _, i := range m[k] {
			r = append(r, xs[i])
		}
	}
	return r
}
