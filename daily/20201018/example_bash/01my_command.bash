my_command() {
    echo $@
}
_my_command_completion(){
    COMPREPLY=( $(compgen -W "tonikaku nanika" -- ${COMP_WORDS[COMP_CWORD]}) );
}
complete -F _my_command_completion my_co
