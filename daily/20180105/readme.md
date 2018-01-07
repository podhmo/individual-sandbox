## sbt 特定のsub projectだけを対象にassemblyを呼ぶ

```
> project <subproject>
> assembly
```

あるいはbuild.sbtで各projectに対して以下をつける

```
aggregate in assembly := false
```

- https://github.com/sbt/sbt-assembly/issues/168

## sbt上で環境変数を変える

```
show envVars
set envVars += 'x' -> y;
show envVars
```

- [sbt - How to set system properties for runMain on command line? - Stack Overflow](https://stackoverflow.com/questions/21389392/how-to-set-system-properties-for-runmain-on-command-line "sbt - How to set system properties for runMain on command line? - Stack Overflow")

## sbt 真面目に理解

- http://www.scala-sbt.org/1.0/docs/ja

### cheat sheet

- sbt new -- scaffold (`sbt new scala/scala-seed.g8`とかする)
- sbt clean -- target以下を消す(target/は.gitignoreすれば良さそう)
- sbt compile -- build
- sbt run -- buildされたコードの実行
- sbt package -- jar作成(ただし自分自身だけ)
- sbt ~testQuick -- 監視してテスト実行(止めるのはenterキー)
- sbt reload -- build.sbtなどを読み直す
- sbt project -- 現在のprojectを変更する


:warning: 毎回JVMの立ち上がりとJITを動かすのは馬鹿馬鹿しいのでsbtは立ち上げっぱなしにする

### ファイルの書き方

- build.sbt
- project/*.sbt

ディレクトリ構造

http://www.scala-sbt.org/1.0/docs/ja/Directories.html


## scalaのこと

- sbt packageで作ったjarのこと
- jar tfとmeta-inf/manifest.mf
- java -cp <jar file> <class>

## sbt-assemblyもうすこし

assemblyとassemblyPackageDependency

```
$ diff -u <(jar tf target/scala-2.10/hello-assembly-1.0.jar | sort) <(jar tf target/scala-2.10/hello-assembly-1.0-deps.jar | sort)
--- /dev/fd/63  2018-01-05 19:22:04.000000000 +0900
+++ /dev/fd/62  2018-01-05 19:22:04.000000000 +0900
@@ -1,10 +1,4 @@
 META-INF/MANIFEST.MF
-foo/
-foo/bar/
-foo/bar/baz/
-foo/bar/baz/Main$.class
-foo/bar/baz/Main$delayedInit$body.class
-foo/bar/baz/Main.class
 library.properties
 rootdoc.txt
 scala/
```

### sbt packageは？

```
sbt package
```

### dependencyを除いてbuild

build.sbtに以下を追加

```
assemblyOption in assembly := (assemblyOption in assembly).value.copy(includeScala = false, includeDependency = false)
```

```
$ sbt assembly
$ jar tf target/scala-2.10/hello-assembly-1.0.jar
META-INF/MANIFEST.MF
foo/
foo/bar/
foo/bar/baz/
foo/bar/baz/Main$.class
foo/bar/baz/Main$delayedInit$body.class
foo/bar/baz/Main.class
```

実行は

```
$ java -cp "target/scala-2.10/hello-assembly-1.0-deps.jar:target/scala-2.10/hello_2.10-1.0.jar" foo.bar.baz.Main
```

## sbtの基礎

ただただ実行するだけ

```
$ sbt
> compile run
```

packageにする

```
$ sbt 
> compile package
```

直接実行しようとするとエラーが出る(scalaのあれこれが見つからない。fat-jarなら大丈夫)

```
$ java -jar target/scala-2.10/example_sbt_2.10-0.1-SNAPSHOT.jar
Error: A JNI error has occurred, please check your installation and try again
Exception in thread "main" java.lang.NoClassDefFoundError: scala/Function0
```

直接scalaコマンドで実行しても良い。

```
$ port contents scala2.10 | grep "bin/scala$"
  /opt/local/share/scala-2.10/bin/scala
$ /opt/local/share/scala-2.10/bin/scala target/scala-2.10/example_sbt_2.10-0.1-SNAPSHOT.jar
```

内部はbashのスクリプトのよう

```
$ bash -x $(port contents scala2.10 | grep "bin/scala$") target/scala-2.10/example_sbt_2.10-0.1-SNAPSHOT.jar
```

こんな感じで実行される

```
$ java -Xmx256M -Xms32M -Xbootclasspath/a:/opt/local/share/scala-2.10/lib/akka-actors.jar:/opt/local/share/scala-2.10/lib/jline.jar:/opt/local/share/scala-2.10/lib/scala-actors-migration.jar:/opt/local/share/scala-2.10/lib/scala-actors.jar:/opt/local/share/scala-2.10/lib/scala-compiler.jar:/opt/local/share/scala-2.10/lib/scala-library.jar:/opt/local/share/scala-2.10/lib/scala-reflect.jar:/opt/local/share/scala-2.10/lib/scala-swing.jar:/opt/local/share/scala-2.10/lib/scalap.jar:/opt/local/share/scala-2.10/lib/typesafe-config.jar -classpath '""' -Dscala.home=/opt/local/share/scala-2.10 -Dscala.usejavacp=true scala.tools.nsc.MainGenericRunner target/scala-2.10/example_sbt_2.10-0.1-SNAPSHOT.jar
```

### fat-jar

fat-jarなら以下だけでOK

```
$ java -jar <fat-jar>.jar
```

sbtのpackageで作られるのはfat-jarではない。fat-jarを作りたければsbt-assemblyを使う。

※ ただしfat-jarのbuildがめちゃくちゃ時間がかかる場合がある。


see

- https://alvinalexander.com/scala/sbt-how-to-compile-run-package-scala-project
- https://stackoverflow.com/questions/24238060/how-to-run-jar-generated-by-package-possibly-with-other-jars-under-lib

## jarの中を覗く

```
$ jar tf <filename>.jar
```

## cassandra

defaultで立ち上げたあと以下のようなエラー。

```
$ cassandra
WARN  03:16:22 JNA link failure, one or more native method will be unavailable.
ERROR 03:16:22 Directory /opt/local/share/java/cassandra2-2.1.3/data/data doesn't exist
ERROR 03:16:22 Has no permission to create /opt/local/share/java/cassandra2-2.1.3/data/data directory
```

/opt/local以下になっているdataの位置を変えたい。

### configuration

```
$ cassandra -f <config file>
```

`data_file_directories` を設定すれば良いっぽい？

### config fileの在り処

cassandra.yamlと言うものが何処かにあるらしい。

```
$ port contents cassandra | grep yaml
  /opt/local/share/java/cassandra2-2.1.3/conf/cassandra-topology.yaml
  /opt/local/share/java/cassandra2-2.1.3/conf/cassandra.yaml
  /opt/local/share/java/cassandra2-2.1.3/conf/metrics-reporter-config-sample.yaml
  /opt/local/share/java/cassandra2-2.1.3/lib/snakeyaml-1.11.jar
```

:warning: ここでのcassandraはportfileを自分で書き換えたもの(https://trac.macports.org/ticket/45004)。

参考

- http://cassandra.apache.org/doc/latest/configuration/cassandra_config_file.html


ここ書き換えるしか無いかも？


### cqlshへつなげるようにする

defaultだとcqlshでの接続にエラーを出す。(cassandra-cliは動く模様)

```
$ cqlsh

Connection error: ('Unable to connect to any servers', {'127.0.0.1': ConnectionException('Did not get expected SupportedMessage response; instead, got: <Error from server: code=0000 [Server error] message="io.netty.handler.codec.DecoderException: org.apache.cassandra.transport.ProtocolException: Invalid or unsupported protocol version: 4">',)})
```

cassandra.yamlのstart_rpcをtrueにする。defaultのrpc_portは9160。

できていたけれど。ダメっぽいしばらくはcassandra-cliを使うことにする。

### keyspaceの作成など

- cassandra-cli使っていけば良い？

setup

古かった2.2.10に上げる

```
$ openssl sha256 /opt/local//var/macports/distfiles/cassandra2/apache-cassandra-2.2.10-src.tar.gz
SHA256(/opt/local//var/macports/distfiles/cassandra2/apache-cassandra-2.2.10-src.tar.gz)= 583d51ebb3d14146f2c8bf7cb69899167bf584c4577b481db4f61d284db8af87
$ openssl rmd160 /opt/local//var/macports/distfiles/cassandra2/apache-cassandra-2.2.10-src.tar.gz
RIPEMD160(/opt/local//var/macports/distfiles/cassandra2/apache-cassandra-2.2.10-src.tar.gz)= 3def651ea8f77139bbd53ed04952862c92312d18
```


### 起動

```
sudo port load cassandra
```
