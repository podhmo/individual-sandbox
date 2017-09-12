# mac ulimit when running out of fd

temporary changes

```
sudo launchctl limit maxfiles 1000000 1000000
```

# go x/sync/errgroup

この辺面白そうなので使っておきたい。

- https://github.com/golang/sync
