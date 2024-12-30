# denoの `--watch-hmr` オプション

denoの `--watch-hmr` オプションの挙動を把握したかった。
ふつうに自動でreloadしてくれるだけなのかな？

## 00

`--watch-hmr` で*.tsファイルの更新は確認できる。でもどうやらhtmlとかの更新確認はしてくれないみたい？

```console
$ deno run -A --watch-hmr 00watch.ts
Watcher Process started.
...
```
