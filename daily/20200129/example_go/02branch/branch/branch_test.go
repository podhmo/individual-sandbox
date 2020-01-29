package branch

import "testing"

func TestDo(t *testing.T) {
	called := false
	use := func(xtype XType) error {
		called = true
		return nil
	}

	cases := []struct {
		msg    string
		xtype  XType
		called bool
	}{
		{
			msg:    "foo",
			xtype:  XTypeFoo,
			called: false,
		},
		{
			msg:    "bar",
			xtype:  XTypeBar,
			called: false,
		},
		{
			msg:    "boo",
			xtype:  XTypeBoo,
			called: true,
		},
	}

	for _, c := range cases {
		c := c
		t.Run(c.msg, func(t *testing.T) {
			err := (&Command{cont: use}).Do(c.xtype)
			if err != nil {
				t.Fatalf("never %+v", err)
			}

			if c.called != called {
				t.Errorf("want %v, but %v", c.called, called)
			}
		})
	}
}
