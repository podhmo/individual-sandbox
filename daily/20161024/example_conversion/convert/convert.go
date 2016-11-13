package convert

import (
	def "github.com/podhmo/hmm/def"
	model "github.com/podhmo/hmm/model"
)

func ConvertFromModelGroup(src *model.Group) (*def.Group, error) {
	if src == nil {
		return nil, nil
	}
	dst := &def.Group{}
	tmp1 := src.Id.Hex()
	tmp2 := def.ID(tmp1)
	dst.ID = &(tmp2)
	dst.Name = &(src.Name)
	return dst, nil
}

func ConvertFromModelPage(src *model.Page) (*def.Page, error) {
	if src == nil {
		return nil, nil
	}
	dst := &def.Page{}
	tmp3 := src.Id.Hex()
	tmp4 := def.ID(tmp3)
	dst.ID = &(tmp4)
	dst.Path = &(src.Path)
	dst.Title = &(src.Title)
	return dst, nil
}

func ConvertFromModelSkill(src *model.Skill) (*def.Skill, error) {
	if src == nil {
		return nil, nil
	}
	dst := &def.Skill{}
	dst.Name = &(src.Name)
	return dst, nil
}

func ConvertFromModelUser(src *model.User) (*def.User, error) {
	if src == nil {
		return nil, nil
	}
	dst := &def.User{}
	tmp5 := string(src.Gender)
	tmp6 := def.Gender(tmp5)
	dst.Gender = &(tmp6)
	tmp7, err := ConvertFromModelGroup(src.Group)
	if err != nil {
		return nil, err
	}
	dst.Group = tmp7
	tmp8 := src.Id.Hex()
	tmp9 := def.ID(tmp8)
	dst.ID = &(tmp9)
	dst.Name = &(src.Name)
	tmp10, err := ConvertFromModelSkillMany(src.Skills)
	if err != nil {
		return nil, err
	}
	dst.Skills = tmp10
	tmp11, err := ConvertFromModelSkillMany(src.Skills2)
	if err != nil {
		return nil, err
	}
	dst.Skills2 = tmp11
	tmp12, err := ConvertFromModelSkillRefMany(src.Skills3)
	if err != nil {
		return nil, err
	}
	dst.Skills3 = tmp12
	return dst, nil
}

func ConvertFromModelSkillMany(src []model.Skill) ([]def.Skill, error) {
	dst := make([]def.Skill, len(src))
	for i, x := range src {
		tmp13 := &(x)
		tmp14, err := ConvertFromModelSkill(tmp13)
		if err != nil {
			return nil, err
		}
		tmp15 := tmp14
		dst[i] = *(tmp15)
	}
	return dst, nil
}

func ConvertFromModelSkillRefMany(src []*model.Skill) ([]*def.Skill, error) {
	dst := make([]*def.Skill, len(src))
	for i, x := range src {
		tmp16, err := ConvertFromModelSkill(x)
		if err != nil {
			return nil, err
		}
		dst[i] = tmp16
	}
	return dst, nil
}
