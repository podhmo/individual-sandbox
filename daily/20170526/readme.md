# unix

ユーザーにグループ追加

```
$ usermod -a <groupname> -G <username>
# 確認
$ cat /etc/groups | grep <groupname>
$ id <username>
```

sudoの編集

visudoを使ったほうが良いらしい(/etc/sudoersを直接編集するより)
ロックしてコピーしてる状態で開き、構文チェックしてから保存するので。

環境変数渡してsudo

```
$ sudo -E <cmd>
```

rootログインの無効化

```
$ sudo passwd -l root
```

su をほぼ無効にする

http://tyru.hatenablog.com/entry/20120325/only_root_can_be_root_via_su

ssh ログインを無効にする

http://kazmax.zpp.jp/linux/lin_ssh.html

```
$ sudo vi /etc/ssh/sshd_config
PermitRootLogin no
```

# arch


以下が動かせる事が重要

- rfkill
- iwtools
- wpa_supplicant


# arch ntp

```
sudo timedatectl set-ntp true
sudo vi /etc/systemd/timesyncd.conf
```

# arch X

```
$ lspci | grep -i VGA  # intelっぽい
$ ADDITIONAL=xf86-video-intel
$ pacman -S --noconfirm xorg-server xorg-xinit xterm ${ADDITIONAL}
```

## xorg-server waylang

```
sudo pacman -S --noconfirm xorg-server-xwayland 
```

# arch gnome

- https://wiki.archlinuxjp.org/index.php/GNOME
- https://wiki.archlinuxjp.org/index.php/%E3%83%87%E3%82%A3%E3%82%B9%E3%83%97%E3%83%AC%E3%82%A4%E3%83%9E%E3%83%8D%E3%83%BC%E3%82%B8%E3%83%A3

```
$ sudo pacman -S --noconfirm gnome gdm
$ sudo pacman -S --noconfirm bash-completion
# 試す
$ sudo systemctl start gdm
# $ sudo systemctl enable gdm
# $ sudo ls -al /etc/systemd/system/
display-manager.service -> /usr/lib/systemd/system/gdm.service
```

## waylandを無効にする

/etc/gdm/custom.conf

```
WaylandEnable=false
```

# arch wireless

```
$ lspci -k | grep -i wifi
$ sudo ip link

# if blocked by Rfkill
$ rfkill list
$ rfkill unblock wifi

$ sudo ip link set wlp2s0 up
$ sudo iw dev wlp2s0 scan
$ sudo wpa_supplicant -i wlp2s0 -c /etc/wpa_supplicant.conf
# $ sudo iw dev wlp2s0 link
$ sudo dhcpd wlp2s
```

# arch networkに繋がる環境に至るまでのsoftwareがなかった場合

また頑張って手動でmountして色々やる

```
fdisk -l
mount /dev/nvme0n1p2 /mnt
# 諸々作業(e.g. netrowking)
arch-chroot /mnt
```

# arch network

# arch japanese input method

## keymap

```
$ localectl status
$ localectl list-keymaps
$ localectl set-keymap --no-convert jp106
$ localectl set-x11-keymap jp
```

```
$ sudo pacman -S fcitx-gtk3 fcitx-mozc
$ fcitx-autostart
```
