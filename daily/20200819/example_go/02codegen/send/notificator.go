package send

import (
	"context"
	"fmt"
	"m/model"
)

type Notificator struct {
	Client Client
}

func (n *Notificator) NotifyRegistered(ctx context.Context, user model.User) error {
	n.Client.Send(
		ctx,
		"#random",
		fmt.Sprintf(`
registered %s
`, user.Name))
	return nil
}

type X struct {
	Name string
}

func (n *Notificator) NotifyCancelled(ctx context.Context, user *model.User, x X, n int) error {
	n.Client.Send(
		ctx,
		"#random",
		fmt.Sprintf(`
cancelled %s
`, user.Name))
	return nil
}
