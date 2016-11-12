package convert

import (
	def "github.com/podhmo/hmm/def"
	model "github.com/podhmo/hmm/model"
)

func ConvertFromModelPage(src *model.Page) (*def.Page, error) {
	dst := &def.Page{}
	tmp1 := src.Id.Hex()
	tmp2 := def.ID(tmp1)
	dst.ID = &(tmp2)
	dst.Path = &(src.Path)
	dst.Title = &(src.Title)
	return dst, nil
}

func ConvertFromModelUser(src *model.User) (*def.User, error) {
	dst := &def.User{}
	tmp3, err := ConvertFromModelGroup(src.Group)
	if err != nil {
		return nil, err
	}
	dst.Group = tmp3
	tmp4 := src.Id.Hex()
	tmp5 := def.ID(tmp4)
	dst.ID = &(tmp5)
	dst.Name = &(src.Name)
	return dst, nil
}

func ConvertFromModelGroup(src *model.Group) (*def.Group, error) {
	dst := &def.Group{}
	tmp6 := src.Id.Hex()
	tmp7 := def.ID(tmp6)
	dst.ID = &(tmp7)
	dst.Name = &(src.Name)
	return dst, nil
}
