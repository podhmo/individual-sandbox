package database

import (
	"database/sql"
	"m/model"
	"time"

	"golang.org/x/xerrors"
)

type PostStore gateway

func (s *PostStore) New(title, body string) *model.Post {
	return &model.Post{
		Created: time.Now().UnixNano(),
		Title:   title,
		Body:    body,
	}
}

func (s *PostStore) Create(posts ...*model.Post) error {
	objs := make([]interface{}, len(posts))
	for i := range posts {
		objs[i] = posts[i]
	}
	if err := s.db.Insert(objs...); err != nil {
		return xerrors.Errorf("Insert failed: %w", err)
	}
	return nil
}

func (s *PostStore) Save(posts ...*model.Post) (int64, error) {
	objs := make([]interface{}, len(posts))
	for i := range posts {
		objs[i] = posts[i]
	}
	c, err := s.db.Update(objs...)
	if err != nil {
		return 0, xerrors.Errorf("Update failed: %w", err)
	}
	return c, nil
}

func (s *PostStore) Count() (int64, error) {
	count, err := s.db.SelectInt("select count(*) from posts")
	if err != nil {
		return 0, xerrors.Errorf("select count(*) failed: %w", err)
	}
	return count, nil
}

func (s *PostStore) SelectOne(postID int64) (*model.Post, error) {
	var p model.Post
	err := s.db.SelectOne(&p, "select * from posts where post_id=?", postID)
	if err != nil {
		return nil, xerrors.Errorf("SelectOne failed: %w", err)
	}
	return &p, nil
}
func (s *PostStore) SelectAll() ([]*model.Post, error) {
	var posts []*model.Post
	_, err := s.db.Select(&posts, "select * from posts order by post_id")
	if err != nil {
		return nil, xerrors.Errorf("Select failed: %w", err)
	}
	return posts, nil
}

func (s *PostStore) Delete(postID int64) (sql.Result, error) {
	c, err := s.db.Exec("delete from posts where post_id=?", postID)
	if err != nil {
		return nil, xerrors.Errorf("Exec failed: %w", err)
	}
	return c, nil
}
