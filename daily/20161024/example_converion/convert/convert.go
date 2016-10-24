package convert

import (
	"github.com/podhmo/hmm/def"
	"github.com/podhmo/hmm/model"
)

func ConvertFromPage(src *model.Page) (*def.Page, error) {
	dst := &def.Page{}
	dst.Title = &(src.Title)
	dst.Path = &(src.Path)
	return dst, nil
}
