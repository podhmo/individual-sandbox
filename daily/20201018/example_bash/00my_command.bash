my_command() {
    echo $@
}
_my_command_completion(){
    COMPREPLY=(tonikaku nanika);
}
complete -F _my_command_completion my_command
