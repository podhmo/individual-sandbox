package main

import (
	"context"
	"encoding/json"
	"fmt"
	"log"
	"os"
	"strconv"
)

type InquiryStatus string

const (
	InquiryStatusNew      InquiryStatus = "New"
	InquiryStatusAssigned               = "Assigned"
	InquiryStatusClosed                 = "Closed"
)

type Inquiry struct {
	Author   User
	Assignee User
	Status   InquiryStatus
}
type User struct {
	Email string
}

type Role string               // e.g. admin, user
type Resource string           // e.g. issue
type Permission string         // e.g. create, update, delete
type ResourcePermission string // e.g. "issue@create"

type RBACStorage struct {
	Roles       []Role
	Permissions map[Resource][]Permission
	Grants      map[Role][]ResourcePermission
	Filters     map[string]Filter
}
type Filter struct {
	Name string
	Func func(ctx context.Context, ob interface{}) error `json:"-"`
}

func (f Filter) MarshalJSON() ([]byte, error) {
	return []byte(strconv.Quote(fmt.Sprintf("Func#%s", f.Name))), nil
}

// https://dev.to/sergey_telpuk/write-rbac-for-nestjs-1ike
// https://dev.to/bastianrob/rbac-in-rest-api-using-go-5gg0

func main() {
	s := RBACStorage{
		Roles: []Role{
			"Manager",
			"Ops",
			"CS",
			"Client",
		},
		Permissions: map[Resource][]Permission{
			"inquiry": []Permission{
				"create",
				"assign",
				"get",
			},
		},
		Grants: map[Role][]ResourcePermission{
			"Client": []ResourcePermission{
				"inquiry@get#filter1", // .createdBy==ctx.email
				"inquiry@create",
				"inquiry@assign",
			},
			"CS": []ResourcePermission{
				"inquiry@get#filter2", // .status==New
				"inquiry@assign",
			},
			"Ops": []ResourcePermission{
				"inquiry@get#filter3", // .assignee==ctx.email
			},
			"Manager": []ResourcePermission{
				"inquiry@get#filter4", // .status==Assigned
				"inquiry@assign",
			},
		},
		Filters: map[string]Filter{
			"filter1": Filter{Name: "filter1", Func: func(ctx context.Context, ob interface{}) error {
				// .createdBy==ctx.email
				email := Get(ctx, "email")
				if email == nil {
					return fmt.Errorf("not found email")
				}
				if ob.(*Inquiry).Author.Email != email {
					return fmt.Errorf("mismatch")
				}
				return nil
			}},
			"filter2": Filter{Name: "filter2", Func: func(ctx context.Context, ob interface{}) error {
				// .status==New
				if ob.(*Inquiry).Status != InquiryStatusNew {
					return fmt.Errorf("unmatch status")
				}
				return nil
			}},
			"filter3": Filter{Name: "filter3", Func: func(ctx context.Context, ob interface{}) error {
				// .assignee==ctx.email
				email := Get(ctx, "email")
				if email == nil {
					return fmt.Errorf("not found email")
				}
				if ob.(*Inquiry).Assignee.Email != email {
					return fmt.Errorf("mismatch")
				}
				return nil
			}},
			"filter4": Filter{Name: "filter4", Func: func(ctx context.Context, ob interface{}) error {
				// .status==Assigned
				if ob.(*Inquiry).Status != InquiryStatusAssigned {
					return fmt.Errorf("unmatch status")
				}
				return nil
			}},
		},
	}

	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	if err := enc.Encode(s); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func Get(ctx context.Context, k string) interface{} {
	return nil
}
