## 時間があったら

- [カーネギーメロンのDBに関する講義が面白いのでおすすめ - だいたいよくわからないブログ](http://matsu-chara.hatenablog.com/entry/2016/12/10/110000)

結局、advent calendarは真面目に読めてない。


## golang date 意外と日付の計算が面倒

```
$ gore
gore version 0.2.6  :help for help
gore> :import time
gore> t, err := time.Parse(time.RFC3339, "2016-01-30T00:00:00Z")
(time.Time)2016-01-30 00:00:00 +0000 UTC
(interface {})<nil>
gore> t.AddDate(0,1,0).Date()
(int)2016
(time.Month)March
(int)1
```

[time.AddDate](https://golang.org/pkg/time/#Time.AddDate)の意味を勘違い。
>  AddDate normalizes its result in the same way that Date does, so, for example, adding one month to October 31 yields December 1, the normalized form for November 31.

[月初/月末取得するやつ](./example_date/calendar)
