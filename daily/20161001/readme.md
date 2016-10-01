# python asyncio 続き

以前のやつマシにする方法がわかったかも。[こちら](../20160929/example_asyncio/)の続き。

# python anaconda のこと

- `$HOME/anaconda` 以下に色々インストールされる。
- `$PATH` の先頭に `$HOME/anaconda/bin` が追加される
- (conda管理外で)拡張ライブラリをbuildするタイミングで、何らかのコマンドの実行結果を解析して利用するものは死ぬ可能性がある。
- もちろん、(conda管理内でも)condaのことが考えられていない拡張ライブラリのビルドスクリプトは依存の条件を満たせなくて死ぬ場合がある

## install後に `$PATH` が更新される

anacondaがインストールされると `~/.bash_profile` に以下の様なものがしれっと追加される。(mac)

```bash
# added by Anaconda3 4.2.0 installer
export PATH="/Users/<username>/anaconda/bin:$PATH"
```

以下であればまだマシなのかも？

```bash
alias activate_anaconda='export PATH=$HOME/anaconda/bin:`echo $PATH | sed "s@$HOME/anaconda/bin:@@g"`'
alias deactivate_anaconda='export PATH=`echo $PATH | sed "s@$HOME/anaconda/bin:@@g"`'
activate_anaconda
```

ついでにvirtualenvみたいに。 `$PS1` などを変更すれば良いのかもしれない。
