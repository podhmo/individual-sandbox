## mongo

こういうのはだめ。

```
db.createCollection("xs");
db.createCollection("ys");
var xids = [ObjectId(), ObjectId()];
db.xs.insert([
  {_id: xids[0], name: "a"},
  {_id: xids[1], name: "b"}
])
db.ys.insert([
  {_id: ObjectId(), name: "i", rels: [{xId: xids[0]}, {xId: xids[1]}]},
  {_id: ObjectId(), name: "j", rels: [{xId: xids[0]}, {xId: xids[1]}]},
  {_id: ObjectId(), name: "k", rels: [{xId: xids[0]}, {xId: xids[1]}]},
])
db.ys.updateMany({"rels.xId": xids[0]}, {"$set": {"rels.xId": null}})
db.xs.remove({_id: xids[0]})
```


## python subprocessのoutput

何故かpythonだけ上手く行かない感じ。irbの方はしっかり返してくれるのだけれど。

## go setup go environment

```
sudo pacman -S go
go get -u -v golang.org/x/tools/cmd/godoc
go get -u -v golang.org/x/tools/cmd/goimports
go get -u -v github.com/golang/lint/golint
go get -u -v github.com/nsf/gocode
go get -u -v github.com/rogpeppe/godef
go get -u -v github.com/k0kubun/pp
go get -u -v github.com/motemen/gore
go get -u -v golang.org/x/tools/cmd/godoc
go get -u -v github.com/podhmo/selfish/cmd/selfish
go get -u -v github.com/podhmo/hatena/cmd/hatena
```

## python setup python environment

~/.config/yapf/style

```
[style]
based_on_style = pep8
column_limit = 100
dedent_closing_brackets=true
spaces_around_power_operator = true
split_arguments_when_comma_terminated = true
join_multiple_lines = false
```

requirements.txt

```
git+https://github.com/podhmo/yayapf
flake8
kamidana
dictknife
``` 

repositories

```
git clone git@github.com:podhmo/kamidana
git clone git@github.com:podhmo/utatane
git clone git@github.com:podhmo/nbreversible
git clone git@github.com:podhmo/prestring
git clone git@github.com:podhmo/dictknife
git clone git@github.com:podhmo/swagger-marshmallow-codegen
```

### TkAgg

```
sudo pacman -Sy tk
```

### GTK3Agg (wip)

matplotlib

```
sudo pacman -Suy python-gobject
pip install matplotlib pygobject pycair
```

~/.config/matplotlib/matplotlibrc

```
# backend : GTK3Agg
backend : TkAgg
```

- https://stackoverflow.com/questions/31324430/installing-pygobject-via-pip-in-virtualenv
- https://matplotlib.org/faq/usage_faq.html

## git config

```
[user]
	name = podhmo
	email = ababjam61+github@gmail.com

[core]
    pager = less -r
	excludesfile = /home/podhmo/.gitignore

[color]
    ui = auto

[alias]
    br = branch
    ba = branch -a
    bm = branch --merged
    bn = branch --no-merged
    ca = commit -a
    ci = commit
    co = checkout
    di = diff
    dis = diff --cached
    fix = commit --amend
    log-graph = log --graph --date=short --pretty=format:'%Cgreen%h%Creset %cd %Cblue%cn %Creset%s'
    ls = ls-files
    sh = stash
    st = status
    sub = submodule
    glog = log --graph --decorate --oneline

[branch "develop"]
   remote = origin
   merge = origin/develop
   rebase = true
[push]
	default = simple
```

