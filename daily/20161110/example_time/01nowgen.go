package main

import (
	"fmt"
	"time"
)

type NowGenUseCase string

const (
	NowGenUseCaseNow = NowGenUseCase("now")
)

type NowGen interface {
	Now() time.Time
	NowWith(NowGenUseCase) time.Time
}

type ActualNowGen struct{}

func (g *ActualNowGen) Now() time.Time {
	return g.NowWith(NowGenUseCaseNow)
}
func (g *ActualNowGen) NowWith(usecase NowGenUseCase) time.Time {
	return time.Now()
}

func NewNowGen() NowGen {
	return &ActualNowGen{}
}

type MockNowGen struct {
	Map  map[NowGenUseCase]time.Time
	Base NowGen
}

func NewMockNowGen() NowGen {
	return &MockNowGen{Base: NewNowGen(), Map: make(map[NowGenUseCase]time.Time)}
}

func (g *MockNowGen) Now() time.Time {
	return g.NowWith(NowGenUseCaseNow)
}
func (g *MockNowGen) NowWith(usecase NowGenUseCase) time.Time {
	v, ok := g.Map[usecase]
	if ok {
		return v
	}
	return g.Base.NowWith(usecase)
}

func ModifyWith(nowgen NowGen, modify func(m map[NowGenUseCase]time.Time), clear bool) {
	switch r := nowgen.(type) {
	case *MockNowGen:
		defer func() {
			if clear {
				r.Map = make(map[NowGenUseCase]time.Time)
			}
		}()
		modify(r.Map)
	}
}

func main() {
	{
		nowgen := NewNowGen()
		fmt.Println(nowgen.Now())
	}
	{
		nowgen := NewMockNowGen()
		ModifyWith(nowgen,
			func(m map[NowGenUseCase]time.Time) {
				fmt.Println("before mock", nowgen.Now())
				mockNow, _ := time.Parse(time.RFC3339, "2000-01-01T00:00:00Z")
				m[NowGenUseCaseNow] = mockNow
				fmt.Println("after mock", nowgen.Now())
			}, true)
		fmt.Println("after clear", nowgen.Now())
	}

}
