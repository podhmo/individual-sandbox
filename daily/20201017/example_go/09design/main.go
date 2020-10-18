package main

type Todo struct {
	Title string `json:"title"`
	Done  bool   `json:"done"`
}

type ListInput struct {
	Offset int `openapi:"query"`
	Limit  int `openapi:"query"`
}

func main() {
	var api API
	router := api.Router
	router.Group("todo", func(router Router) {
		router.Endpoint("GET", "/todo", func(input struct {
			ListInput
		}) []Todo {
			return nil
		})
	})
}
