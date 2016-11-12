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
	tmp3 := string(src.Gender)
	tmp4 := def.Gender(tmp3)
	dst.Gender = &(tmp4)
	tmp5, err := ConvertFromModelGroup(src.Group)
	if err != nil {
		return nil, err
	}
	dst.Group = tmp5
	tmp6 := src.Id.Hex()
	tmp7 := def.ID(tmp6)
	dst.ID = &(tmp7)
	dst.Name = &(src.Name)
	return dst, nil
}

func ConvertFromModelGroup(src *model.Group) (*def.Group, error) {
	dst := &def.Group{}
	tmp8 := src.Id.Hex()
	tmp9 := def.ID(tmp8)
	dst.ID = &(tmp9)
	dst.Name = &(src.Name)
	return dst, nil
}
