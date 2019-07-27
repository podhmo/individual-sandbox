package replay

import "testing"

func Test(t *testing.T) {
	doReplay := ReplayWith(WithForceUpdate())

	getData := func() string {
		return "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
	}

	got := getData()
	if want := doReplay(t, got); got != want {
		t.Errorf("want %q, but %q", want, got)
	}
}
