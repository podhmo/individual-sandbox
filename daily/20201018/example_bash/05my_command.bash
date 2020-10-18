function my_command() {
    echo $@
}
function _my_completion_function(){
  local cur prev cword
  _get_comp_words_by_ref -n : cur prev cword
  case ${cword} in
      1 )
          args="hello byebye";;
      2 )
          args="${prev}ooo xxx yyyy";;
  esac
  COMPREPLY=( `compgen -W "$args" -- ${COMP_WORDS[COMP_CWORD]}` );
}
complete -F _my_completion_function my_command
