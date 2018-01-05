## sbt scala sbtのversionを調べる

```
sbt sbt-version
```

もしくは

```
> sbt-version
```

## sbt assembly

sbt-assemblyは色々まとまったjarを一気に作ってくれるものっぽい

## sbt の依存関係を見る

https://stackoverflow.com/questions/25519926/how-see-dependency-tree-in-sbt

project/plugin.sbt で

```
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.8.2")
```

その後

```
sbt dependencyTree
```

