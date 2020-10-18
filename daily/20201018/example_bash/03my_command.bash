my_command() {
    echo $@
}
_my_completion_function(){
    local args
    case $COMP_CWORD in
    1 )
        args=(foo bar boo);;
    2 )
        args=(xxx yyy zzz);;
    esac
    COMPREPLY=( `compgen -W "$args" -- ${COMP_WORDS[COMP_CWORD]}` );
}
complete -F _my_completion_function my_command
