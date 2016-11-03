package convert

import (
	"github.com/podhmo/hmm/def"
	"github.com/podhmo/hmm/model"
)

func ConvertFromModelPage(src *model.Page) (*def.Page, error) {
	dst := &def.Page{}
	tmp0 := def.ID(src.Id)
	dst.ID = &(tmp0)
	dst.Path = &(src.Path)
	dst.Title = &(src.Title)
	return dst, nil
}
func ConvertFromDefPage(src *def.Page) (*model.Page, error) {
	dst := &model.Page{}
	dst.Id = string(*(src.ID))
	dst.Path = *(src.Path)
	// FIXME: PathHash is not found
	dst.Title = *(src.Title)
	return dst, nil
}
