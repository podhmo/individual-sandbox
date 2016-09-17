# python matplotlib widgetを使いたい

-[radio button](http://matplotlib.org/examples/widgets/radio_buttons.html)

# python matplotlibを pyobjcで使ってみる

~/.matplotib/matplotlibrc
```
backend: CocoaAgg  # pyobjc
```

```
# installされていなければ
sudo port install py35-pyobjc py35-pyobjc-cocoa
sudo port clean py35-matplotlib
sudo port install py35-matplotlib
```

まだダメっぽい。

- [python - Symbol not found: _PyObject_REPR when calling pyobjc function, Python3.5.1 - Stack Overflow](
http://stackoverflow.com/questions/34696875/symbol-not-found-pyobject-repr-when-calling-pyobjc-function-python3-5-1)

macportのあれが古いっぽい

- [MacPorts Guide -- creating portfile](https://guide.macports.org/#development.creating-portfile)
- [howto/Upgrade – MacPorts](https://trac.macports.org/wiki/howto/Upgrade)


手動
```
sudo port edit py35-pyobjc-cocoa # version変える -> 3.1.1
sudo port -v checksum py35-pyobjc-cocoa  # error
wget `port distfile py35-pyobjc-cocoa | grep pypi`
openssl dgst -rmd160 pyobjc* | cut -d " " -f 2 | pbcopy
# 8404c8064adc0157b40de29eccb8ae20236ce256
openssl dgst -sha256 pyobjc* | cut -d " " -f 2 | pbcopy
# b5b21f0c89c33967ea1a842deabdb3118e72324d28844983bab9414f88b7b090
sudo port edit py35-pyobjc-cocoa # sha256,rmd160変える
```

推奨

```
$ cd `port dir py35-pyobjc-cocoa`
$ sudo cp -p Portfile Portfile.orig
# versionを変更
$ sudo port edit py35-pyobjc-cocoa
$ sudo port -d checksum py35-pyobjc-cocoa
# distのchecksumの方に書き換える
$ sudo port edit py35-pyobjc-cocoa
$ sudo port -d install py35-pyobjc-cocoa

$ cd `port dir py35-pyobjc`
$ sudo cp -p Portfile Portfile.orig
# versionを変更
$ sudo port edit py35-pyobjc
$ sudo port -d checksum py35-pyobjc
$ sudo port -d livecheck py35-pyobjc
# distのchecksumの方に書き換える
$ sudo port edit py35-pyobjc
$ sudo port -d install py35-pyobjc
```


# macports logを見る

```
sudo port edit <package>
sudo port logfile <package>
sudo port log <package>
sudo port notes <package>
port location <package>
port contents <package>
```
