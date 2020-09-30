package internal

import (
	"log"
	"net/http"
	"os"

	"github.com/podhmo/tenuki/capture"
	"golang.org/x/xerrors"
	"google.golang.org/api/tasks/v1"
)

func init() {
	capture.DefaultDumper = &capture.FileDumper{BaseDir: capture.Dir("/tmp/xxx")}
}

func NewStore(client *http.Client) (*Store, error) {
	// inner:
	client.Transport = &capture.CapturedTransport{
		Transport: client.Transport,
		Printer:   log.New(os.Stdout, "", 0),
	}
	// outer:
	// if t, ok := client.Transport.(*oauth2.Transport); ok {
	// 	t.Base = &tenuki.CapturedTransport{
	// 		T: tenuki.ToLogf(log.New(os.Stdout, "store", 0)),
	// 	}
	// }

	srv, err := tasks.New(client)
	if err != nil {
		return nil, xerrors.Errorf("Unable to retrieve tasks Client %w", err)
	}
	p := &serviceProvider{srv: srv}
	return &Store{
		TodoList: &TodoListStore{serviceProvider: p},
		Todo:     &TodoStore{serviceProvider: p},
	}, nil
}

type Store struct {
	TodoList *TodoListStore
	Todo     *TodoStore
}

type serviceProvider struct {
	srv *tasks.Service
}

func (p *serviceProvider) provideService() *tasks.Service {
	return p.srv
}

type TodoList struct {
	ID    string
	Title string

	raw interface{} `json:"-"`
}

type Todo struct {
	ID      string
	Title   string
	Status  string
	Updated string

	raw interface{} `json:"-"`
}

type TodoListStore struct {
	*serviceProvider
}

func (s *TodoListStore) List() ([]TodoList, error) {
	// todo: handling limit
	srv := s.provideService()

	r, err := srv.Tasklists.List().MaxResults(10).Do()
	if err != nil {
		return nil, xerrors.Errorf("Unable to retrieve task lists. %w", err)
	}

	items := make([]TodoList, len(r.Items))
	for i, x := range r.Items {
		items[i] = TodoList{
			Title: x.Title,
			ID:    x.Id,
			raw:   x,
		}
	}
	return items, nil
}

type TodoStore struct {
	*serviceProvider
}

func (s *TodoStore) List(listID string) ([]Todo, error) {
	// todo: handling limit
	srv := s.provideService()

	r, err := srv.Tasks.List(listID).MaxResults(10).Do()
	if err != nil {
		return nil, xerrors.Errorf("Unable to retrieve tasks. %w", err)
	}

	items := make([]Todo, len(r.Items))
	for i, x := range r.Items {
		items[i] = Todo{
			ID:      x.Id,
			Title:   x.Title,
			Status:  x.Status,
			Updated: x.Updated,
			raw:     x,
		}
	}
	return items, nil
}
