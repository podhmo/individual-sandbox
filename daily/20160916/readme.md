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
sudo port install python35 +readline
sudo port select python python35
sudo port select python3 python35
sudo port install py35-numpy py35-scipy py35-matplotlib
```

## 仮想環境

```
# もし必要なら
python3.5 -m ensurepip --user
echo 'export PATH=~/Library/Python/3.5/bin:$PATH' >> ~/.bash_profile
python -m venv --system-site-packages viz
```

## RuntimeError: Python is not installed as a framework

virtualenvで作ってしまうと、frameworkとしてinstallされたpythonと認識されないのでダメ。

```
$ virtualenv --python=`which python3.5` --system-site-packages viz
$ . viz/bin/activate
$ python example_pyplot/00sin.py
    from matplotlib.backends import _macosx
RuntimeError: Python is not installed as a framework. The Mac OS X backend will not be able to function correctly if Python is not installed as a framework. See the Python documentation for more information on installing Python as a framework on Mac OS X. Please either reinstall Python as a framework, or try one of the other backends. If you are Working with Matplotlib in a virtual enviroment see 'Working with Matplotlib in Virtual environments' in the Matplotlib FAQ
```

これは以下の部分でのエラー。
https://github.com/matplotlib/matplotlib/blob/v1.5.3/src/_macosx.m#L6285


### 手抜きの解決方法(macosxのbackendを使わない)

```
echo "backend: WebAgg" >> ~/.matplotlib/matplotlibrc
```

一時的で良いなら環境変数で与えても良い。

```
$ MPLBACKEND=WebAgg python foo
```

http://matplotlib.org/faq/environment_variables_faq.html#envvar-MPLBACKEND

# python realtime plotting

- [Realtime plotting | hardsoftlucid](https://hardsoftlucid.wordpress.com/various-stuff/realtime-plotting/)
- https://gist.github.com/podhmo/e57af31389990776c981169700a442db

