# matplotlib 日本語の表示

https://openbook4.me/sections/1674

~/.matplotlib/matplotlibrc に以下を追加

```
font.family: AppleGothic
```

良さそうなttfフォントは以下の様な感じで調べられる。

```python
import matplotlib.font_manager as f
for line in f.fontManager.ttflist: print(line)
```
