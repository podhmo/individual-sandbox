## python unicode escape

decode

```python
import codecs

codecs.decode(b, "unicode-escape")
```

encode
```
ascii(s)
```

hmm

```
*** UnicodeEncodeError: 'utf-8' codec can't encode characters in position 14019-14020: surrogates not allowed
```

## docker

dockerのprocess全部止める

```
docker ps -a -q | xargs docker stop
```

## chromium

```
$ man chromium
$ all_proxy=http://localost:8080 chromium --user-data-dir $HOME/.config/chromium/twitter
```

### 起動時のオプションの把握

chrome://version

を開いて、その中のCommand Lineの部分を見る。

```
/usr/lib/chromium/chromium --profile-directory=twitter --app-id=edcifkpoamnkimdpjiabhfjahoinbpbp --flag-switches-begin --enable-features=DesktopPWAWindowing --flag-switches-end
```
## arch chromeのPWAの位置

$HOME/.local/share/desktop-directories/chrome-apps.directory

## chrome

Can you run multiple Chrome instances with different proxies?

- account分ける
- profile switch omega入れる

## chrome 複数プロファイル

多重起動には２つの方法があるみたい

- profileを分ける
- user-data-dirを分ける

### profileを分ける

chromeで

```
chrome://settings
```

にアクセスしてユーザーを追加する

その後 `--profile-directory <profile>` でchromeを起動

## emacs diredなどで開いた時に勝手に別windowで開かれる挙動

`pop-up-windows` ?

- https://emacs.stackexchange.com/questions/29670/how-to-prevent-some-new-buffers-from-splitting-the-window
