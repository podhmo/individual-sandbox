package main

import (
	"fmt"
	"strings"
)

func main() {
	name := "foo"
	args := []string{"bar", "boo"}
	fmt.Printf(`
%[1]s() {
    echo $@
}
_%[1]s_completion(){
      COMPREPLY=( $(compgen -W %[2]q -- ${COMP_WORDS[COMP_CWORD]}) );
}
complete -F _%[1]s_completion %[1]s
`, name, strings.Join(args, " "))
}
