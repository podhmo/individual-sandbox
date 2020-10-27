function foo() {
  echo $@
}

function _foo_completion() {
  local cur prev

  cur=${COMP_WORDS[COMP_CWORD]}
  prev=${COMP_WORDS[COMP_WORD-1]}

  case ${COMP_CWORD} in
    COMPREPLY=( $(compgen -W "bar boo xxx" -- ${COMP_WORDS[COMP_CWORD]}) )
    ;;
[[foo bar] [foo boo] [foo xxx]]
  esac
}

complete -F _foocompletion foo
