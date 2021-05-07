// +build gen

package main

type Params struct {
	// Name 名前
	Name string `arg:"name" required:true`

	// ShowFullName フルネームで表示するオプション
	ShowFullName bool `arg:"show-full-name"`
}

func main() {
	structgen.GenMain(
		Params,
		structgen.WithEnvVars(),
	)
}
