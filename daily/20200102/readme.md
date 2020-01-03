# macports 雑

## update

```
port selfupdate
port rev-upgrade
```

## いい感じのなにかを探す

```
port echo updated
port echo requested
port echo leaves
```

## 不要ななにかを消す

```
# 使ってない枝葉のポートをuninstall
sudo port -uf uninstall leaves

# 依存も含めてuninstall
sudo port uninstall --follow-dependents mysql postgresql

# アップデート後に残った古いversionをuninstall
$ sudo port uninstall inactive
```

