# golang 時間のmock

良いものがほしい

# ubuntu ntp 時刻があっていない場合

```
sudo apt-get install ntp
ntpq -p
```

# ubuntuの入力環境が使いにくい理由

ハードウェア的な話

- zenbookのキーボードはそもそも入力しづらい(手への抵抗が大きい)
- エンターキーが遠い。エンターキーの入力ミスが頻発。

gnome-shell的な話

- launcher的なアプリの反応が鈍い
- 手軽に辞書が引けない
- gnome-terminalが不要に高機能かつコピー貼り付けの際のショートカットキーに互換性がない

emacs

- emacsで日本語とアルファベットのフォントサイズが違いすぎる
- デフォルトのフォントが見やすくない
- C-;に謎の機能がついている(fcitxと干渉)
- 数字などの変換が半角優先になっていない
- emacsのキーバインドで文字の変換候補の選択ができない
- デフォルトでは不要な入力モードが存在してしまう(e.g. カタカナ入力など)

環境全体の話

- 至るところでemacsのキーバインドで移動したい
- virtual desktopがない？(多分設定で有効にしないとダメ)

(virtualboxの問題)

- key repeatを早めても快適にはならない

## したほうが良いこと

- 真面目にフォントの設定をする
- キーボードのマッピングを変える(無変換->backspace, 変換->半角全角, カタカナひらがな -> enter)

# まだ設定が必要なこと

bashの設定ファイルは持っていたほうが良い気がしてきた

- gitのaliasなどの設定が不足
- vimperatorのcopy.jsの設定
- screenrc
- openコマンドが貧弱な状態

# ubuntuのこと

- エディタのデフォルトがnano
- dpkg-reconfigureやupdate-alternativesで何を入力すれば良いのか忘れてしまっている
- 至るところでemacs key bindingがつかいたい

## emacs key binding

UbuntuのキーバインドをEmacs風にする - Qiita
http://qiita.com/wamei/items/a83680a3ad9b2f8f8dcc

## デフォルトのエディタの変更

```
update-alternatives --get-selections | grep -i editor
sudo update-alternatives --list editor
sudo update-alternatives --config editor
```


