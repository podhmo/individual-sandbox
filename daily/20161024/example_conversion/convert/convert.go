package convert

import (
	def "github.com/podhmo/hmm/def"
	model "github.com/podhmo/hmm/model"
)

// FromModelGroupToDefGroup : converts model.Group -> def.Group
func FromModelGroupToDefGroup(src *model.Group) (*def.Group, error) {
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

// FromModelPageToDefPage : converts model.Page -> def.Page
func FromModelPageToDefPage(src *model.Page) (*def.Page, error) {
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

// FromModelSkillToDefSkill : converts model.Skill -> def.Skill
func FromModelSkillToDefSkill(src *model.Skill) (*def.Skill, error) {
	if src == nil {
		return nil, nil
	}
	dst := &def.Skill{}
	dst.Name = &(src.Name)
	return dst, nil
}

// FromModelTeamToDefTeam : converts model.Team -> def.Team
func FromModelTeamToDefTeam(src *model.Team) (*def.Team, error) {
	if src == nil {
		return nil, nil
	}
	dst := &def.Team{}
	tmp5 := src.Id.Hex()
	tmp6 := def.ID(tmp5)
	dst.ID = &(tmp6)
	dst.Name = &(src.Name)
	tmp7, err := FromModelUserRefManyToDefUserMany(src.Users)
	if err != nil {
		return nil, err
	}
	dst.Users = tmp7
	return dst, nil
}

// FromModelUserToDefUser : converts model.User -> def.User
func FromModelUserToDefUser(src *model.User) (*def.User, error) {
	if src == nil {
		return nil, nil
	}
	dst := &def.User{}
	tmp8 := string(src.Gender)
	tmp9 := def.Gender(tmp8)
	dst.Gender = &(tmp9)
	tmp10, err := FromModelGroupToDefGroup(src.Group)
	if err != nil {
		return nil, err
	}
	dst.Group = tmp10
	tmp11 := src.Id.Hex()
	tmp12 := def.ID(tmp11)
	dst.ID = &(tmp12)
	dst.Name = &(src.Name)
	tmp13, err := FromModelSkillManyToDefSkillMany(src.Skills)
	if err != nil {
		return nil, err
	}
	dst.Skills = tmp13
	tmp14, err := FromModelSkillManyToDefSkillMany(src.Skills2)
	if err != nil {
		return nil, err
	}
	dst.Skills2 = tmp14
	tmp15, err := FromModelSkillRefManyToDefSkillRefMany(src.Skills3)
	if err != nil {
		return nil, err
	}
	dst.Skills3 = tmp15
	return dst, nil
}

// FromModelUserRefManyToDefUserMany : converts []*model.User -> []def.User
func FromModelUserRefManyToDefUserMany(src []*model.User) ([]def.User, error) {
	dst := make([]def.User, len(src))
	for i, x := range src {
		tmp16, err := FromModelUserToDefUser(x)
		if err != nil {
			return nil, err
		}
		tmp17 := tmp16
		dst[i] = *(tmp17)
	}
	return dst, nil
}

// FromModelSkillManyToDefSkillMany : converts []model.Skill -> []def.Skill
func FromModelSkillManyToDefSkillMany(src []model.Skill) ([]def.Skill, error) {
	dst := make([]def.Skill, len(src))
	for i, x := range src {
		tmp18 := &(x)
		tmp19, err := FromModelSkillToDefSkill(tmp18)
		if err != nil {
			return nil, err
		}
		tmp20 := tmp19
		dst[i] = *(tmp20)
	}
	return dst, nil
}

// FromModelSkillRefManyToDefSkillRefMany : converts []*model.Skill -> []*def.Skill
func FromModelSkillRefManyToDefSkillRefMany(src []*model.Skill) ([]*def.Skill, error) {
	dst := make([]*def.Skill, len(src))
	for i, x := range src {
		tmp21, err := FromModelSkillToDefSkill(x)
		if err != nil {
			return nil, err
		}
		dst[i] = tmp21
	}
	return dst, nil
}
