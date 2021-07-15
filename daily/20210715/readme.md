## python docker `--cache-dir` をdocker build時にも

これの続き

- [pythonのバッチ用のイメージを作りたくなったのでサイズがどれくらいになるか調べてみた - podhmo's diary](https://pod.hatenablog.com/entry/2021/02/27/170020)

buildkitを使うとして このときcacheのイメージは消えるのかどうかが知りたい。

- [python - Using a pip cache directory in docker builds - Stack Overflow](https://stackoverflow.com/questions/58018300/using-a-pip-cache-directory-in-docker-builds)
- [Speed up pip downloads in Docker with BuildKit’s new caching](https://pythonspeed.com/articles/docker-cache-pip-downloads/)

これの他に `--cache-from` が使える？

### 追記

BUILDKIT_INLINE_CACHE を使う方法もあるらしいのだけれど、まだこちらはよくわかっていない。

あー、なるほどね。image中の行部分もcacheとして利用できるみたいな感じ。

```console
RUN python3 -m pip install boto3 pandas
```

を

```console
RUN python3 -m pip install boto3 pandas streamlit
```

にすると効かないけれど

```console
RUN python3 -m pip install boto3 pandas
RUN python3 -m pip install streamlit
```

これは効く。
