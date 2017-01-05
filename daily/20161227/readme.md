# python marshmallow 整理

- many=Trueはschema用のメタデータ
- load時のdefaultはmissing
- dump時のdefaultはdefault
- (それぞれcallbableがおける)
- collectionに対するfieldの定義はfields.Listを使う

# arch linux

とりあえずinstallする。usbが2つ必要かも。

- [インストールガイド - ArchWiki](https://wiki.archlinuxjp.org/index.php/%E3%82%A4%E3%83%B3%E3%82%B9%E3%83%88%E3%83%BC%E3%83%AB%E3%82%AC%E3%82%A4%E3%83%89)

## pacmanの使い方を調べたい感じ

ミラーの最適化

```
sudo pacman -g
```

レポジトリのアップデート

```
sudo pacman -Syy
```

レポジトリのアップデートをワークツリーに反映

```
sudo pacman -Syyu
```

## 検索など

search remote

```
pacman -Ss <name>
```

searach local

```
pacman -Qs <name>
```

listing local

```
packman -Ql
```

## 依存関係

```
pctree
```

## install

install 

```
sudo pacman -S <name>
```

fetch only

```
sudo pacman -Sw <name>
```

## uninstall

```
sudo pacman -R <name>

# with full dependencies
sudo pacman -Rs <name>

# purge
sudo pacman -Rns <name>
```

## clear chache

```
sudo pacman -Sc
```

### refs

http://qiita.com/MoriokaReimen/items/dbe1448ce6c0f80a6ac1
