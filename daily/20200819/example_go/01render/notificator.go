package main

import (
	"context"
	"fmt"
)

type Notificator struct {
	Client Client
}

func (n *Notificator) NotifyRegistered(ctx context.Context, user User) error {
	n.Client.Send(
		ctx,
		"#random",
		fmt.Sprintf(`
registered %s
`, user.Name))
	return nil
}

func (n *Notificator) NotifyCancelled(ctx context.Context, user User) error {
	n.Client.Send(
		ctx,
		"#random",
		fmt.Sprintf(`
cancelled %s
`, user.Name))
	return nil
}
