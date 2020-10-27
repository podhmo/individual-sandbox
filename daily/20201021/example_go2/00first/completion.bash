function foo(){
    echo $@
}
function _foo_completion(){
      COMPREPLY=( $(compgen -W "bar boo" -- ${COMP_WORDS[COMP_CWORD]}) );
}
complete -F _foo_completion foo
