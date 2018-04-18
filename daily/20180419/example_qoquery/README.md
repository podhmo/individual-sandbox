gocon2018-springとgolang.tokyo#14の参加者のリストつくるやつ

```console
$ make -B
go run 00extract/main.go "https://golangtokyo.connpass.com/event/82723/participation/#participants" | tee golang.tokyo.csv
...
go run 00extract/main.go "https://gocon.connpass.com/event/82515/participation/#participants" | tee gocon.csv
...
make create-list
make[1]: Entering directory '.'
go run 01list/main.go -c ~/.config/egoist/config.json --username podhmo --listname go3 *.csv
2018/04/19 02:30:06 get user..
2018/04/19 02:30:06 got user id is 28082543
2018/04/19 02:30:06 get target list..
2018/04/19 02:30:06 got target list go3
2018/04/19 02:30:06 open gocon.csv
2018/04/19 02:30:07 add users to list gocon.csv 50
2018/04/19 02:30:09 add users to list gocon.csv 50
2018/04/19 02:30:11 add users to list gocon.csv 50
2018/04/19 02:30:12 add users to list gocon.csv 50
2018/04/19 02:30:14 add users to list gocon.csv 50
2018/04/19 02:30:15 add users to list gocon.csv 50
2018/04/19 02:30:16 add users to list gocon.csv 22
2018/04/19 02:30:17 open golang.tokyo.csv
2018/04/19 02:30:18 add users to list golang.tokyo.csv 50
2018/04/19 02:30:19 add users to list golang.tokyo.csv 50
2018/04/19 02:30:20 add users to list golang.tokyo.csv 15
make[1]: Leaving directory '.'
```
