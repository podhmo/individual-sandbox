

function foo() {
    echo $@
}
function _foo_completion(){
    local cur prev

    cur=${COMP_WORDS[COMP_CWORD]}
    prev=${COMP_WORDS[COMP_CWORD-1]}

    case ${COMP_CWORD} in
        1)

	COMPREPLY=( $(compgen -W 'bar boo bxxx xxx' -- ${COMP_WORDS[COMP_CWORD]}) )
	;;
  
    esac
}
complete -F _foo_completion foo
