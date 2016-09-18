# python python実行した後にstty saneが必要

- stty saneするのめんどくさい

- [osx - OS-X terminal behaves oddly after running Python interactively - Super User](http://superuser.com/questions/983755/os-x-terminal-behaves-oddly-after-running-python-interactively)

readlineのoptionが入っていないのでは？

```
sudo port clean python35
sudo port install python35 +readline
```

ok

# python pyplot matplotlib

見た目は `ggplot` のstyleを使うと綺麗

```python
import matplotlib.pyplot as plt

plt.style.use('ggplot')
```

利用できるstyleを調べるにはstylelibにあるファイルを探してみると良い。defaultでは以下のようなstyleが用意されている。

- [matplotlib/lib/matplotlib/mpl-data/stylelib at master · matplotlib/matplotlib](https://github.com/matplotlib/matplotlib/tree/master/lib/matplotlib/mpl-data/stylelib)

# python pyplot

tutorialに書かれていることだけでも整理しておきたい気持ちがあるな。

- [Pyplot tutorial — Matplotlib 1.5.3 documentation](http://matplotlib.org/users/pyplot_tutorial.html)


```python
import numpy as np
import matplotlib.pyplot as plt

"""
複数の図を表示する
"""


def f(t):
    # e^-t * cos(2πt)
    return np.exp(-t) * np.cos(2 * np.pi * t)


t1 = np.arange(0.0, 5.0, 0.1)
t2 = np.arange(0.0, 5.0, 0.02)

plt.figure(1)
plt.grid(True)

# 3つのsubplot areaをfigure 1に用意。その1つ目のsubplot areaを使う
plt.subplot(311)
plt.grid(True)
plt.plot(t1, f(t1), 'bo', t2, f(t2), 'k')

plt.subplot(312)
plt.plot(t2, np.cos(2 * np.pi * t2), 'r--')

plt.subplot(313)
plt.plot(t2, np.exp(-t2), 'g--')

plt.show()
```
