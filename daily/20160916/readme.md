# golang iotaの表示

iotaの表示Stringerにしなくてもそれなりに見分けがついたっけ？

```
type status int
const (
    valid = 1 + iota
    invalid
)
```

この時 validは 1とかただの値が出るだけ。 %Tつければ型は分かるけれど。

# shell mac psでcpu使用率

手軽にlocalの環境でCPUなどの使用率を記録したい場合

```
$ ps -o '%cpu,%mem,pid,rss,time,command'
```

# mac numpyとかの環境

```
sudo port install python35
sudo port select python python35
sudo port select python3 python35
sudo port install py35-numpy py35-scipy py35-matplotlib
```

## virtualenv

```
# もし必要なら
python3.5 -m ensurepip --user
echo 'export PATH=~/Library/Python/3.5/bin:$PATH' >> ~/.bash_profile
virtualenv --python=`which python3.5` --system-site-packages viz
```

## RuntimeError: Python is not installed as a framework

### 手抜きの解決方法

```
echo "backend: CocoaAgg" >> ~/.matplotlib/matplotlibrc
```

# python realtime plotting

- [Realtime plotting | hardsoftlucid](https://hardsoftlucid.wordpress.com/various-stuff/realtime-plotting/)


