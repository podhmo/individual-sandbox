package notification

import (
	"context"
	"m/07separated/slackclient"

	"golang.org/x/xerrors"
)

type Notificator struct {
	*slackclient.Client
}

func New(driver slackclient.Driver) *Notificator {
	return &Notificator{Client: slackclient.New(driver)}
}

func (n *Notificator) NotifyHelloMessage(ctx context.Context) error {
	if err := n.Driver().SendMessage(ctx, "#random", `
hello, this is test.
`); err != nil {
		return xerrors.Errorf("send: %w", err)
	}
	return nil
}

func (n *Notificator) NotifyByebyeMessage(ctx context.Context) error {
	if err := n.Driver().SendMessage(ctx, "#randomm", `
byebye, this is test.
`); err != nil {
		return xerrors.Errorf("send: %w", err)
	}
	return nil
}
