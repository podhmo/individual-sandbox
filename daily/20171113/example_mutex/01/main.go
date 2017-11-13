package main

import (
	"fmt"
	"math/rand"
	"sync"
)

type S struct {
	Value0  int
	Value1  int
	Value2  int
	Value3  int
	Value4  int
	Value5  int
	Value6  int
	Value7  int
	Value8  int
	Value9  int
	Value10 int
	Value11 int
	Value12 int
	Value13 int
	Value14 int
	Value15 int
	Value16 int
	Value17 int
	Value18 int
	Value19 int
	Value20 int
	Value21 int
	Value22 int
	Value23 int
	Value24 int
	Value25 int
	Value26 int
	Value27 int
	Value28 int
	Value29 int
	Value30 int
	Value31 int
	Value32 int
	Value33 int
	Value34 int
	Value35 int
	Value36 int
	Value37 int
	Value38 int
	Value39 int
	Value40 int
	Value41 int
	Value42 int
	Value43 int
	Value44 int
	Value45 int
	Value46 int
	Value47 int
	Value48 int
	Value49 int
	Value50 int
	Value51 int
	Value52 int
	Value53 int
	Value54 int
	Value55 int
	Value56 int
	Value57 int
	Value58 int
	Value59 int
	Value60 int
	Value61 int
	Value62 int
	Value63 int
	Value64 int
	Value65 int
	Value66 int
	Value67 int
	Value68 int
	Value69 int
	Value70 int
	Value71 int
	Value72 int
	Value73 int
	Value74 int
	Value75 int
	Value76 int
	Value77 int
	Value78 int
	Value79 int
	Value80 int
	Value81 int
	Value82 int
	Value83 int
	Value84 int
	Value85 int
	Value86 int
	Value87 int
	Value88 int
	Value89 int
	Value90 int
	Value91 int
	Value92 int
	Value93 int
	Value94 int
	Value95 int
	Value96 int
	Value97 int
	Value98 int
	Value99 int
}

func main() {
	var wg sync.WaitGroup
	var m sync.Mutex
	s := S{}
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value0 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value1 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value2 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value3 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value4 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value5 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value6 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value7 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value8 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value9 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value10 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value11 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value12 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value13 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value14 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value15 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value16 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value17 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value18 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value19 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value20 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value21 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value22 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value23 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value24 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value25 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value26 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value27 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value28 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value29 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value30 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value31 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value32 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value33 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value34 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value35 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value36 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value37 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value38 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value39 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value40 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value41 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value42 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value43 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value44 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value45 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value46 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value47 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value48 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value49 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value50 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value51 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value52 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value53 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value54 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value55 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value56 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value57 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value58 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value59 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value60 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value61 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value62 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value63 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value64 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value65 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value66 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value67 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value68 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value69 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value70 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value71 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value72 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value73 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value74 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value75 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value76 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value77 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value78 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value79 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value80 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value81 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value82 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value83 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value84 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value85 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value86 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value87 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value88 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value89 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value90 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value91 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value92 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value93 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value94 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value95 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value96 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value97 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value98 = rand.Int()
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		m.Lock()
		defer m.Unlock()
		s.Value99 = rand.Int()
		wg.Done()
	}()
	wg.Wait()
	fmt.Printf("%#+v\n", s)
}
