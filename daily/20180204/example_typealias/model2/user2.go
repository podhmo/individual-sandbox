package model2

import (
    "../model"
)

type User2 = model.User

// cannot define new methods on non-local type model.User
// func (u *User2) Hello() string {
//     return "hello from model2"
// }
