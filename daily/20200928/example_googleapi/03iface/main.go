package main

// https://developers.google.com/tasks/quickstart/go
import (
	"fmt"
	"log"
	"m/03iface/internal"
	"os"

	"golang.org/x/xerrors"
)

func main() {
	if err := run(); err != nil {
		log.Fatalf("!! %+v", err)
	}
}

func run() error {
	filename := os.Getenv("GOOGLE_CREDENTIALS")
	if filename == "" {
		filename = "credentials.json"
	}

	factory := internal.NewTasksClientFactory(filename, "token.json")
	client := factory.Client()

	store, err := internal.NewStore(client)
	if err != nil {
		return xerrors.Errorf("new store %w", err)
	}

	fmt.Println("Task Lists:")
	var listIDs []string
	todolists, err := store.TodoList.List()
	if err != nil {
		return xerrors.Errorf("list todo list: %w", err)
	}

	for _, i := range todolists {
		fmt.Printf("\t%s (%s)\n", i.Title, i.ID)
		listIDs = append(listIDs, i.ID)
	}

	for _, listID := range listIDs {
		fmt.Println("Task Todos:", listID)
		todos, err := store.Todo.List(listID)
		if err != nil {
			return xerrors.Errorf("list todo: %w", err)
		}
		for _, i := range todos {
			// pp.Println(i)
			fmt.Printf("\t%-15s%s\n", i.Status, i.Title) // updated
		}
	}
	return nil
}
