## python prompt_toolkit 確認しておきたいもの

- stdinを別画面で受け取りつつ、stdout,stderrを出力
- progress barをasyncioで
- 複数progress barをasyncioで

### progressbar

```python
with ProgressBar(
    title=HTML("<b>Example of many parallel tasks.</b>"),
    bottom_toolbar=HTML("<b>[Control-L]</b> clear  <b>[Control-C]</b> abort"),
) as pb:
    label, total, sleep_time = "First task", 50, 0.1
    for i in pb(range(total), label=label):
        time.sleep(sleep_time)
```

どうやら、ProgressBar.appにapplicationを持つらしい。`__enter__()`は何なんだろう。
とても色んなことをしていた。


