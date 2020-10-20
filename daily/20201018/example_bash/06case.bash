set -u -e -x

function foo() {
    case $1,$2 in
        foo,-* )
            echo foo flag;;
        foo,* )
            echo foo args;;
        * )
            echo other
    esac
}

foo foo -x
foo foo bar
foo
