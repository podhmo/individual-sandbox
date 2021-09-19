package main

import "context"

type Provider struct {
}

func (p *Provider) DB(ctx context.Context) *DB {
	return &DB{}
}

type DB struct {
}

func main() {
	ctx := context.Background()
	p := &Provider{}

	db := p.DB(ctx)
}
