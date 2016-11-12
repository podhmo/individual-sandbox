package convert

import (
	def "github.com/podhmo/hmm/def"
	model "github.com/podhmo/hmm/model"
	bson "gopkg.in/mgo.v2/bson"
)

func ConvertFromModelPage(src *model.Page) (*def.Page, error) {
	dst := &def.Page{}
	tmp0 := src.Id.Hex()
	tmp1 := def.ID(tmp0)
	dst.ID = &(tmp1)
	dst.Path = &(src.Path)
	dst.Title = &(src.Title)
	return dst, nil
}

func ConvertFromDefPage(src *def.Page) (*model.Page, error) {
	dst := &model.Page{}
	tmp2 := string(*(src.ID))
	dst.Id = bson.ObjectIdHex(tmp2)
	dst.Path = *(src.Path)
	// FIXME: PathHash is not found
	dst.Title = *(src.Title)
	return dst, nil
}

func ConvertFromModelUser(src *model.User) (*def.User, error) {
	dst := &def.User{}
	// FIXME: Group is not found
	tmp3 := src.Id.Hex()
	tmp4 := def.ID(tmp3)
	dst.ID = &(tmp4)
	dst.Name = &(src.Name)
	return dst, nil
}
