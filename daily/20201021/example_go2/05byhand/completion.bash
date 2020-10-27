function foo() {
    echo $@
}
function _foo_completion(){
    local first profile datapath
    profile="default"
    cmdname="${COMP_WORDS[0]}"
    datapath="$HOME/.config/compgen/${profile}/${cmdname}"

    local filepath
    case ${COMP_CWORD} in
        1)
            filepath="${datapath}/${cmdname}"
            if [ -f ${filepath} ]; then
                COMPREPLY=( $(compgen -W "`cat ${filepath}`" -- ${COMP_WORDS[COMP_CWORD]}) )
            else
                COMPREPLY=( $(compgen -f -- ${COMP_WORDS[COMP_CWORD]}))
            fi
            ;;
        2)
            filepath="${datapath}/${cmdname}.${COMP_WORDS[1]}"
            if [ -f ${filepath} ]; then
                COMPREPLY=( $(compgen -W "`cat ${filepath}`" -- ${COMP_WORDS[COMP_CWORD]}) )
            else
                COMPREPLY=( $(compgen -f -- ${COMP_WORDS[COMP_CWORD]}))
            fi
            ;;
        *)
            COMPREPLY=( $(compgen -f -- ${COMP_WORDS[COMP_CWORD]}))
            ;;
    esac
}
complete -o nosort -F _foo_completion foo
