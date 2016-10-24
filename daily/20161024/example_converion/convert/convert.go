package convert

import (
	"github.com/podhmo/hmm/def"
	"github.com/podhmo/hmm/model"
)

func ConvertFromPage(src *model.Page) (*def.Page, error) {
	dst := &def.Page{}
	dst.Path = &(src.Path)
	dst.Title = &(src.Title)
	return dst, nil
}
