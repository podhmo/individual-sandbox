# python 謎の順序で並んでいるものから順序の規則を見つけたい


## python xlsxをcsvに

こういうのこそgo製で欲しい

```
pip install xlsx2csv
xlsx2csv -a <>.xlsx
xlsx2csv -s 1 <>.xlsx
```


## capture process on OSX (OSX doen't have `/proc`)

http://stackoverflow.com/questions/3425340/how-can-i-capture-the-stdout-from-a-process-that-is-already-running

```
capture() {
    sudo dtrace -p "$1" -qn '
        syscall::write*:entry
        /pid == $target && arg0 == 1/ {
            printf("%s", copyinstr(arg1, arg2));
        }
    '
}
```
