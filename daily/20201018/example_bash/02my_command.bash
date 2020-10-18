my_command() {
    echo $@
}
_my_command_completion(){
    local args=`ls *.html`
    COMPREPLY=( `compgen -W "$args" -- ${COMP_WORDS[COMP_CWORD]}` );
}
complete -F _my_command_completion my_comma
