
function foo() {
    echo $@
}
function _foo_completion(){
    local cur prev

    cur=${COMP_WORDS[COMP_CWORD]}
    prev=${COMP_WORDS[1]}

    case ${COMP_CWORD} in
        1)

            COMPREPLY=( $(compgen -W 'bar boo bxxx xxx' -- ${COMP_WORDS[COMP_CWORD]}) )
            ;;
        2)
            case ${prev} in
                bar )

                    COMPREPLY=( $(compgen -W 'one two three bar' -- ${COMP_WORDS[COMP_CWORD]}) )
                    ;;
                boo )

                    COMPREPLY=( $(compgen -W 'one two three boo' -- ${COMP_WORDS[COMP_CWORD]}) )
                    ;;
                bxxx )

                    COMPREPLY=( $(compgen -W 'one two three bxxx' -- ${COMP_WORDS[COMP_CWORD]}) )
                    ;;
                xxx )

                    COMPREPLY=( $(compgen -W 'one two three xxx' -- ${COMP_WORDS[COMP_CWORD]}) )
                    ;;
            esac
    esac
}
complete -F _foo_completion foo
