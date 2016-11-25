package now

import "time"

type nowHandler struct {
	now *time.Time
}

var currentNowHandler *nowHandler

func init() {
	currentNowHandler = &nowHandler{}
}

func (h *nowHandler) Now() time.Time {
	if h.now != nil {
		return *h.now
	}
	return time.Now()
}

// Now : 現在時刻を返す。 FIXME: あとで他のパッケージで現在時刻を利用しているものを元に治す
func Now() time.Time {
	return currentNowHandler.Now()
}

// FixedNow : 固定された時刻を返す。
func FixedNow(layout, s string) func() time.Time {
	fixed, _ := time.Parse(layout, s)
    h := &nowHandler{now: &fixed}
    return h.Now
}

// WithFixedNow : 渡された関数内ではnow.Nowを固定された時刻で計算する
func WithFixedNow(layout, s string, fn func()){
    prev := currentNowHandler.now
    defer func(){ currentNowHandler.now = prev }()
	fixed, _ := time.Parse(layout, s)
    currentNowHandler.now = &fixed
    fn()
}
