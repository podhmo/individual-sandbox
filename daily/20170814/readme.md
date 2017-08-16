## vimperator copy.js firefox

[here](../20160914/readme.md)


## disable ipv6 on wifi

```
sudo sysctl net.ipv6.conf.wlp2s0.disable_ipv6=1
ip a
```

## arch aur pacman pacaur

- https://github.com/rmarquis/pacar


```
pacaur -S emacs-mozc
```

failed

手動で入れ直した。キャッシュ自体は `~/.cache/pacaur` 以下にある。

```lisp
(require 'mozc)
(set-language-environment 'japanese)
(setq default-input-method 'japanese-mozc)
```

(mozc-key-event-to-key-and-modifiers 14)
(mozc-key-event-to-key-and-modifiers 'down)

