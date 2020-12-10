package iface

import "m/03iface/internal"

type Getter interface {
	Get(k string) string
}
type GetSet interface {
	Getter
	internal.Setter
}
