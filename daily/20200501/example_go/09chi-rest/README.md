# github.com/go-chi/chi

Welcome to the chi/_examples/rest generated docs.

## Routes

<details>
<summary>`/`</summary>

- [RequestID]()
- [Logger]()
- [Recoverer]()
- [URLFormat]()
- [SetContentType.func1]()
- **/**
	- _GET_
		- [main.main.func1]()

</details>
<details>
<summary>`/admin/*`</summary>

- [RequestID]()
- [Logger]()
- [Recoverer]()
- [URLFormat]()
- [SetContentType.func1]()
- **/admin/***
	- [main.AdminOnly]()
	- **/**
		- _GET_
			- [main.adminRouter.func1]()

</details>
<details>
<summary>`/admin/*/accounts`</summary>

- [RequestID]()
- [Logger]()
- [Recoverer]()
- [URLFormat]()
- [SetContentType.func1]()
- **/admin/***
	- [main.AdminOnly]()
	- **/accounts**
		- _GET_
			- [main.adminRouter.func2]()

</details>
<details>
<summary>`/admin/*/users/{userId}`</summary>

- [RequestID]()
- [Logger]()
- [Recoverer]()
- [URLFormat]()
- [SetContentType.func1]()
- **/admin/***
	- [main.AdminOnly]()
	- **/users/{userId}**
		- _GET_
			- [main.adminRouter.func3]()

</details>
<details>
<summary>`/articles/*`</summary>

- [RequestID]()
- [Logger]()
- [Recoverer]()
- [URLFormat]()
- [SetContentType.func1]()
- **/articles/***
	- **/**
		- _GET_
			- [main.paginate]()
			- [main.ListArticles]()
		- _POST_
			- [main.CreateArticle]()

</details>
<details>
<summary>`/articles/*/search`</summary>

- [RequestID]()
- [Logger]()
- [Recoverer]()
- [URLFormat]()
- [SetContentType.func1]()
- **/articles/***
	- **/search**
		- _GET_
			- [main.SearchArticles]()

</details>
<details>
<summary>`/articles/*/{articleID}/*`</summary>

- [RequestID]()
- [Logger]()
- [Recoverer]()
- [URLFormat]()
- [SetContentType.func1]()
- **/articles/***
	- **/{articleID}/***
		- [main.ArticleCtx]()
		- **/**
			- _PUT_
				- [main.UpdateArticle]()
			- _DELETE_
				- [main.DeleteArticle]()
			- _GET_
				- [main.GetArticle]()

</details>
<details>
<summary>`/articles/*/{articleSlug:[a-z-]+}`</summary>

- [RequestID]()
- [Logger]()
- [Recoverer]()
- [URLFormat]()
- [SetContentType.func1]()
- **/articles/***
	- **/{articleSlug:[a-z-]+}**
		- _GET_
			- [main.ArticleCtx]()
			- [main.GetArticle]()

</details>
<details>
<summary>`/panic`</summary>

- [RequestID]()
- [Logger]()
- [Recoverer]()
- [URLFormat]()
- [SetContentType.func1]()
- **/panic**
	- _GET_
		- [main.main.func3]()

</details>
<details>
<summary>`/ping`</summary>

- [RequestID]()
- [Logger]()
- [Recoverer]()
- [URLFormat]()
- [SetContentType.func1]()
- **/ping**
	- _GET_
		- [main.main.func2]()

</details>

Total # of routes: 10

