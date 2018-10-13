## fcitx

```
/usr/lib/mozc/mozc_tool --mode=config_dialog

activate/deactivate
```

## bluetoth keyboard
```console
$ sudo pacman -Sy bluez bluez-utils
$ sudo systemctl start bluetooth.service
# sudo systemctl enable bluetooth.service
```

- [Bluetooth キーボード - ArchWiki](https://wiki.archlinux.jp/index.php/Bluetooth_%E3%82%AD%E3%83%BC%E3%83%9C%E3%83%BC%E3%83%89 "Bluetooth キーボード - ArchWiki")
- [Linux PCにBluetooth機器を接続する - Qiita](https://qiita.com/shskwmt/items/fffabf521201f5835214 "Linux PCにBluetooth機器を接続する - Qiita")

### hmm

```console
$ sudo bluetoothctl
power on
Failed to set power on: org.bluez.Error.Blocked
```

なんか上手くいかない

```console
$ sudo rfkill list
1: phy0: Wireless LAN
        Soft blocked: no
        Hard blocked: no
98: hci0: Bluetooth
        Soft blocked: yes
        Hard blocked: no
$ sudo rfkill unblock 98
```

ok

[Bluetooth doesn't fully turn on · Issue #180 · linrunner/TLP](https://github.com/linrunner/TLP/issues/180 "Bluetooth doesn't fully turn on · Issue #180 · linrunner/TLP"
)

```console
$ sudo bluetoothctl
[bluetooth]#rch)`powe': power on
Changing power on succeeded
[CHG] Device 2C:26:17:0B:7B:7D RSSI: -62
[bluetooth]#rch)`scan': scan on
Discovery started
[bluetooth]# list
Controller A0:C5:89:0B:FB:CF localhost [default]
[bluetooth]# agent KeyboardOnly
Agent is already registered
[bluetooth]# default-agent
Default agent request successful
[bluetooth]# pairable on
[bluetooth]# devices
Device 64:0A:F9:67:F4:98 64-0A-F9-67-F4-98
Device 2C:26:17:07:BE:9F 2C-26-17-07-BE-9F
Device 2C:26:17:0B:7B:7D OMVR-V190
[bluetooth]# info 64:0A:F9:67:F4:98 
Device 64:0A:F9:67:F4:98 (random)
        Alias: 64-0A-F9-67-F4-98
        Paired: no
        Trusted: no
        Blocked: no
        Connected: no
        LegacyPairing: no
        RSSI: -73
[bluetooth]# pair 64:0A:F9:67:F4:98 
Attempting to pair with 64:0A:F9:67:F4:98
[CHG] Device 64:0A:F9:67:F4:98 Connected: yes
Failed to pair: org.bluez.Error.AuthenticationFailed
[CHG] Device 64:0A:F9:67:F4:98 Connected: no
[DEL] Device 64:0A:F9:67:F4:98 64-0A-F9-67-F4-98
[NEW] Device 64:0A:F9:67:F4:98 64-0A-F9-67-F4-98
[bluetooth]# trust 64:0A:F9:67:F4:98 
[CHG] Device 64:0A:F9:67:F4:98 Trusted: yes
Changing 64:0A:F9:67:F4:98 trust succeeded
[bluetooth]# connect 64:0A:F9:67:F4:98 
Attempting to connect to 64:0A:F9:67:F4:98
[bluetooth]# exit
``
