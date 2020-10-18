function my_command() {
    echo $@
}
function _my_completion_function(){
    local args
    case $COMP_CWORD in
    1 )
        args="hello byebye";;
    2 )
        args="${COMP_WORDS[${COMP_CWORD}-1]}ooo xxx yyyy";;
    esac
    COMPREPLY=( `compgen -W "$args" -- ${COMP_WORDS[COMP_CWORD]}` );
}
complete -F _my_completion_function my_command
