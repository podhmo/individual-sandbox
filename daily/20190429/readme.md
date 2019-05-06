## arch download source

- https://stackoverflow.com/questions/4468447/how-to-download-source-code-with-pacman-on-arch-linux
- https://unix.stackexchange.com/questions/14858/in-arch-linux-how-can-i-find-out-which-package-to-install-that-will-contain-file

- rfkillが入っているパッケージを探す
- netctlのsourceをdownloadする

```
$ yay -S asp
$ yay -Fy
$ yay -Fs rfkill
core/netctl 1.20-1
    usr/lib/netctl/rfkill
core/util-linux 2.33.2-1
    usr/bin/rfkill
    usr/share/bash-completion/completions/rfkill
$ asp export netctl
$ cd netctl
$ makepkg -o
```

## makepkg one or more PGP signatures could not be verified

https://qiita.com/hassiyu/items/041dfec87d87f0c914db

```console
$ makepkg -o
==> Validating source files with md5sums...
    netctl-1.20.tar.xz ... Passed
    netctl-1.20.tar.xz.sig ... Passed
==> Verifying source file signatures with gpg...
    netctl-1.20.tar.xz ... FAILED (unknown public key 6D1655C14CE1C13E)
==> ERROR: One or more PGP signatures could not be verified!
```


## python dotenv

自分でコードを書かずに済ませたい場合

```console
$ pip install python-dotenv
```

書いた

https://pod.hatenablog.com/entry/2019/04/29/164109

## bluetooth のキーボードが上手く動かない？

```console
$ sudo bluetoothctl
[bluetooth]# default-agent
Default agent request successful
[bluetooth]# scan on
No default controller available
```

hmm

```console
$ sudo systemctl status bluetooth
● bluetooth.service - Bluetooth service
   Loaded: loaded (/usr/lib/systemd/system/bluetooth.service; enabled; vendor preset: disabled)
   Active: active (running) since Wed 2019-04-10 20:21:48 JST; 2 weeks 5 days ago
     Docs: man:bluetoothd(8)
 Main PID: 908 (bluetoothd)
   Status: "Running"
    Tasks: 1 (limit: 4915)
   Memory: 2.4M
   CGroup: /system.slice/bluetooth.service
           └─908 /usr/lib/bluetooth/bluetoothd

 4月 29 19:03:51 localhost bluetoothd[908]: Failed to add UUID: Invalid Index (0x11)
 4月 29 19:03:51 localhost bluetoothd[908]: Failed to add UUID: Invalid Index (0x11)
 4月 29 19:03:51 localhost bluetoothd[908]: Failed to add UUID: Invalid Index (0x11)
 4月 29 19:03:51 localhost bluetoothd[908]: Failed to add UUID: Invalid Index (0x11)
 4月 29 19:03:51 localhost bluetoothd[908]: Failed to set mode: Invalid Index (0x11)
 4月 29 19:03:51 localhost bluetoothd[908]: Failed to set mode: Invalid Index (0x11)
 4月 29 19:03:51 localhost bluetoothd[908]: Failed to set mode: Invalid Index (0x11)
 4月 29 19:03:51 localhost bluetoothd[908]: Failed to set mode: Invalid Index (0x11)
 4月 29 19:03:52 localhost bluetoothd[908]: Endpoint unregistered: sender=:1.167 path=/MediaEn>
 4月 29 19:03:52 localhost bluetoothd[908]: Endpoint unregistered: sender=:1.167 path=/MediaEn>
lines 1-21/21 (END)
```

そもそもなんかbluetoothのdeviceが認識されていない感。


```
$ sudo rfkill list
1: phy0: Wireless LAN
        Soft blocked: no
        Hard blocked: no
```

[../20181013/readme.md](../20181013/readme.md)

と結果が異なる。

lsmodではみつかる

```console
$ lsmod | grep -i blue
bluetooth             651264  15 btrtl,hidp,btintel,btbcm,bnep,btusb,rfcomm
ecdh_generic           24576  1 bluetooth
rfkill                 28672  9 asus_wmi,bluetooth,cfg80211
crc16                  16384  2 bluetooth,ext4
```

- https://bbs.archlinux.org/viewtopic.php?id=213841

hmm

```console
$ lspci -knn | grep -i blue
$ lspci -knn | grep -i net
02:00.0 Network controller [0280]: Intel Corporation Wireless 8260 [8086:24f3] (rev 3a)
        Subsystem: Intel Corporation Wireless 8260 [8086:8010]
        Kernel driver in use: iwlwifi
        Kernel modules: iwlwifi
```

```console
$ journalctl -b | grep -i bluetooth | grep hci
```

なんかtimeoutしたあと復帰してなさげ

```
Apr 29 19:03:51 localhost kernel: Bluetooth: hci0: command 0x2005 tx timeout
Apr 29 19:03:51 localhost dbus-daemon[1429]: [session uid=1000 pid=1429] Activating via systemd: service name='org.bluez.obex' unit='dbus-org.bluez.obex.service' requested by ':1.2538' (uid=1000 pid=4370 comm="gnome-control-center bluetooth ")
Apr 29 19:03:51 localhost systemd[1414]: Starting Bluetooth OBEX service...
Apr 29 19:03:51 localhost systemd[1414]: Started Bluetooth OBEX service.
Apr 29 19:03:51 localhost bluetoothd[908]: Failed to set mode: Failed (0x03)
Apr 29 19:03:51 localhost bluetoothd[908]: Failed to set mode: Invalid Index (0x11)
Apr 29 19:03:51 localhost bluetoothd[908]: Failed to set mode: Invalid Index (0x11)
Apr 29 19:03:51 localhost bluetoothd[908]: Failed to set mode: Invalid Index (0x11)
Apr 29 19:03:51 localhost bluetoothd[908]: Failed to set mode: Invalid Index (0x11)
Apr 29 19:03:51 localhost bluetoothd[908]: Failed to set mode: Invalid Index (0x11)
Apr 29 19:03:51 localhost bluetoothd[908]: Failed to add UUID: Invalid Index (0x11)
Apr 29 19:03:51 localhost bluetoothd[908]: Failed to add UUID: Invalid Index (0x11)
Apr 29 19:03:51 localhost bluetoothd[908]: Failed to add UUID: Invalid Index (0x11)
Apr 29 19:03:51 localhost bluetoothd[908]: Failed to add UUID: Invalid Index (0x11)
Apr 29 19:03:51 localhost bluetoothd[908]: Failed to add UUID: Invalid Index (0x11)
Apr 29 19:03:51 localhost bluetoothd[908]: Failed to add UUID: Invalid Index (0x11)
Apr 29 19:03:51 localhost bluetoothd[908]: Failed to add UUID: Invalid Index (0x11)
Apr 29 19:03:51 localhost bluetoothd[908]: Failed to set mode: Invalid Index (0x11)
Apr 29 19:03:51 localhost bluetoothd[908]: Failed to set mode: Invalid Index (0x11)
Apr 29 19:03:51 localhost bluetoothd[908]: Failed to set mode: Invalid Index (0x11)
Apr 29 19:03:51 localhost bluetoothd[908]: Failed to set mode: Invalid Index (0x11)
...
Apr 29 22:20:23 localhost org.gnome.Shell.desktop[1462]: Window manager warning: 0x2800010 (Bluetooth ) appears to be one of the offending windows with a timestamp of 215157688.  Working around...
```

```
$ journalctl -f
 Apr 29 22:34:19 localhost dbus-daemon[1393]: [session uid=1000 pid=1393] Activating via systemd: service name='org.bluez.obex' unit='dbus-org.bluez.obex.service' requested by ':1.87' (uid=1000 pid=2609 comm="gnome-control-center ")
 Apr 29 22:34:19 localhost systemd[1378]: Starting Bluetooth OBEX service...
 Apr 29 22:34:19 localhost obexd[2617]: OBEX daemon 5.50
 Apr 29 22:34:19 localhost dbus-daemon[1393]: [session uid=1000 pid=1393] Successfully activated service 'org.bluez.obex'
 Apr 29 22:34:19 localhost systemd[1378]: Started Bluetooth OBEX service.
```

もしかしてkernel moduleが読み込まれていない。

```
$ sudo modprobe -v btusb
insmod /lib/modules/5.0.0-arch1-1-ARCH/kernel/crypto/ecdh_generic.ko.xz 
insmod /lib/modules/5.0.0-arch1-1-ARCH/kernel/net/bluetooth/bluetooth.ko.xz 
insmod /lib/modules/5.0.0-arch1-1-ARCH/kernel/drivers/bluetooth/btintel.ko.xz 
insmod /lib/modules/5.0.0-arch1-1-ARCH/kernel/drivers/bluetooth/btbcm.ko.xz 
insmod /lib/modules/5.0.0-arch1-1-ARCH/kernel/drivers/bluetooth/btrtl.ko.xz 
insmod /lib/modules/5.0.0-arch1-1-ARCH/kernel/drivers/bluetooth/btusb.ko.xz reset=1 
$ sudo systemctl restart bluetooth
$ sudo bluetoothctl
```

https://stackoverflow.com/questions/48279646/bluetoothctl-no-default-controller-available
https://www.reddit.com/r/archlinux/comments/7chx7v/bluetooth_no_default_controller_available/

modinfo?

```
$ lsmod | grep bluetooth
bluetooth             651264  11 btrtl,btintel,btbcm,bnep,btusb
ecdh_generic           24576  1 bluetooth
rfkill                 28672  9 asus_wmi,bluetooth,cfg80211
crc16                  16384  2 bluetooth,ext4
$ lsmod | grep bt
btusb                  57344  0
btrtl                  20480  1 btusb
btbcm                  16384  1 btusb
btintel                28672  1 btusb
bluetooth             651264  11 btrtl,btintel,btbcm,bnep,btusb

## modinfoでの補完
bt3c_cs      bt878        btmrvl       btrfs        bttv         
bt819        btbcm        btmrvl_sdio  btrsi        btusb        
bt856        btcoexist    btmtkuart    btrtl        btwilink     
bt866        btintel      btqca        btsdio 
```


- https://unix.stackexchange.com/questions/330769/how-to-get-bluetooth-working-on-arch-linux
- https://unix.stackexchange.com/questions/247195/bluetooth-scan-does-not-find-any-bluetooth-device
- https://askubuntu.com/questions/632336/bluetooth-broadcom-43142-isnt-working/632348#632348

なんかdeadになった

```
$ sudo systemctl status bluetooth
● bluetooth.service - Bluetooth service
   Loaded: loaded (/usr/lib/systemd/system/bluetooth.service; enabled; vendor preset: disabled)
   Active: inactive (dead)
     Docs: man:bluetoothd(8)

 4月 29 22:59:45 localhost.localdomain systemd[1]: Condition check resulted in Bluetooth service being skipped.
 4月 29 23:00:19 localhost systemd[1]: Condition check resulted in Bluetooth service being skipped.
 4月 29 23:08:37 localhost systemd[1]: Condition check resulted in Bluetooth service being skipped.
```

- https://bbs.archlinux.org/viewtopic.php?id=244532

hmm

```console
$ journalctl -b
4月 29 22:59:41 localhost kernel: ACPI: HPET id: 0x8086a201 base: 0xfed00000
 4月 29 22:59:41 localhost kernel: [Firmware Bug]: TSC_DEADLINE disabled due to Errata; please update microcode to version: 0x52 (or la>
```

hmm

```
$ journalctl -b | grep -i blue
 4月 29 22:59:43 localhost NetworkManager[427]: <info>  [1556546383.6449] Loaded device plugin: NMBluezManager (/usr/lib/NetworkManager/1.18.0-1/libnm-device-plugin-bluetooth.so)
 4月 29 22:59:45 localhost.localdomain dbus-daemon[426]: [system] Activating via systemd: service name='org.bluez' unit='dbus-org.bluez.service' requested by ':1.151' (uid=120 pid=1237 comm="/usr/bin/pulseaudio --daemonize=no ")
 4月 29 22:59:45 localhost.localdomain systemd[1]: Condition check resulted in Bluetooth service being skipped.
 4月 29 23:00:10 localhost dbus-daemon[426]: [system] Failed to activate service 'org.bluez': timed out (service_start_timeout=25000ms)
 4月 29 23:00:10 localhost pulseaudio[1237]: E: [pulseaudio] bluez5-util.c: GetManagedObjects() failed: org.freedesktop.DBus.Error.NoReply: Did not receive a reply. Possible causes include: the remote application did not send a reply, the message bus security policy blocked the reply, the reply timeout expired, or the network connection was broken.
 4月 29 23:00:10 localhost pulseaudio[1520]: E: [pulseaudio] bluez5-util.c: GetManagedObjects() failed: org.freedesktop.DBus.Error.TimedOut: Failed to activate service 'org.bluez': timed out (service_start_timeout=25000ms)
 4月 29 23:00:19 localhost dbus-daemon[426]: [system] Activating via systemd: service name='org.bluez' unit='dbus-org.bluez.service' requested by ':1.428' (uid=1000 pid=2466 comm="/usr/lib/chromium/chromium --profile-directory=Def")
 4月 29 23:00:19 localhost systemd[1]: Condition check resulted in Bluetooth service being skipped.
 4月 29 23:00:44 localhost dbus-daemon[426]: [system] Failed to activate service 'org.bluez': timed out (service_start_timeout=25000ms)
```

hmm

```
$ journalctl -S "2019-04-01 00:00:00" -U "2019-04-28 00:00:00" | grep -i bluetooth | grep -i "invalid index" | wc
      0       0       0
$ journalctl -S "2019-04-28 00:00:00" | grep -i bluetooth | grep -i "invalid index" | wc
     16     192    1376
```

## FILCO

```
4月 29 18:43:51 localhost systemd-logind[402]: Watching system buttons on /dev/input/event17 (FILCO Bluetooth Keyboard)
 4月 29 18:43:51 localhost systemd-logind[402]: Watching system buttons on /dev/input/event19 (FILCO Bluetooth Keyboard System Control)
 4月 29 18:43:51 localhost systemd-logind[402]: Watching system buttons on /dev/input/event18 (FILCO Bluetooth Keyboard Consumer Control)
```

これがいなくなっている。

```
 4月 29 19:03:49 localhost kernel: usb 1-8: reset full-speed USB device number 3 using xhci_hcd
 4月 29 19:03:49 localhost kernel: usb 1-8: device descriptor read/64, error -71
 4月 29 19:03:50 localhost kernel: usb 1-8: device descriptor read/64, error -71
 4月 29 19:03:50 localhost kernel: usb 1-8: reset full-speed USB device number 3 using xhci_hcd
 4月 29 19:03:50 localhost kernel: usb 1-8: device descriptor read/64, error -71
 4月 29 19:03:50 localhost kernel: usb 1-8: device descriptor read/64, error -71
 4月 29 19:03:50 localhost kernel: usb 1-8: reset full-speed USB device number 3 using xhci_hcd
 4月 29 19:03:50 localhost kernel: usb 1-8: Device not responding to setup address.
 4月 29 19:03:51 localhost kernel: usb 1-8: Device not responding to setup address.
 4月 29 19:03:51 localhost kernel: usb 1-8: device not accepting address 3, error -71
 4月 29 19:03:51 localhost kernel: usb 1-8: reset full-speed USB device number 3 using xhci_hcd
 4月 29 19:03:51 localhost kernel: usb 1-8: Device not responding to setup address.
 4月 29 19:03:51 localhost kernel: Bluetooth: hci0: command 0x2005 tx timeout
 4月 29 19:03:51 localhost dbus-daemon[1429]: [session uid=1000 pid=1429] Activating via systemd: service name='org.bluez.obex' unit='dbus-org.bluez.obex.service' requested by ':1.2538' (uid=1000 pid=4370 comm="gnome-control-center bluetooth ")
 4月 29 19:03:51 localhost systemd[1414]: Starting Bluetooth OBEX service...
 4月 29 19:03:51 localhost obexd[4379]: OBEX daemon 5.50
 4月 29 19:03:51 localhost dbus-daemon[1429]: [session uid=1000 pid=1429] Successfully activated service 'org.bluez.obex'
 4月 29 19:03:51 localhost systemd[1414]: Started Bluetooth OBEX service.
 4月 29 19:03:51 localhost kernel: usb 1-8: Device not responding to setup address.
 4月 29 19:03:51 localhost bluetoothd[908]: Failed to set mode: Failed (0x03)
 4月 29 19:03:51 localhost bluetoothd[908]: Failed to set mode: Invalid Index (0x11)
```

- https://superuser.com/questions/1310775/bluetooth-adapter-not-detected-on-linux

```
● dbus.service - D-Bus System Message Bus
   Loaded: loaded (/usr/lib/systemd/system/dbus.service; static; vendor preset: disabled)
   Active: active (running) since Mon 2019-04-29 22:59:42 JST; 1h 57min ago
     Docs: man:dbus-daemon(1)
 Main PID: 426 (dbus-daemon)
    Tasks: 1 (limit: 4915)
   Memory: 4.8M
   CGroup: /system.slice/dbus.service
           └─426 /usr/bin/dbus-daemon --system --address=systemd: --nofork --nopidfile --systemd-activation --syslog-only

 4月 29 22:59:49 localhost.localdomain dbus-daemon[426]: [system] Activation via systemd failed for unit 'dbus-org.freedesktop.resolve1.service': Unit dbus-org.freedesktop.resolve1.service not found.
 4月 29 22:59:52 localhost.localdomain dbus-daemon[426]: [system] Activating via systemd: service name='org.freedesktop.UDisks2' unit='udisks2.service' requested by ':1.260' (uid=1000 pid=1539 comm="/usr/lib/gvfs-udisks2-volume-monitor ")
 4月 29 22:59:52 localhost.localdomain dbus-daemon[426]: [system] Successfully activated service 'org.freedesktop.UDisks2'
 4月 29 23:00:10 localhost dbus-daemon[426]: [system] Failed to activate service 'org.bluez': timed out (service_start_timeout=25000ms)
 4月 29 23:00:19 localhost dbus-daemon[426]: [system] Activating via systemd: service name='org.bluez' unit='dbus-org.bluez.service' requested by ':1.428' (uid=1000 pid=2466 comm="/usr/lib/chromium/chromium --profile-directory=Def")
 4月 29 23:00:44 localhost dbus-daemon[426]: [system] Failed to activate service 'org.bluez': timed out (service_start_timeout=25000ms)
 4月 30 00:52:40 localhost dbus-daemon[426]: [system] Activating via systemd: service name='org.freedesktop.hostname1' unit='dbus-org.freedesktop.hostname1.service' requested by ':1.1756' (uid=0 pid=32345 comm="/usr/lib/bluetooth/bluetoothd ")
 4月 30 00:52:41 localhost dbus-daemon[426]: [system] Successfully activated service 'org.freedesktop.hostname1'
 4月 30 00:53:15 localhost dbus-daemon[426]: [system] Rejected send message, 1 matched rules; type="error", sender=":1.283" (uid=1000 pid=1520 comm="/usr/bin/pulseaudio --daemonize=no ") interface="(unset)" member="(unset)" error name="org.freedesktop.DBus.Error.UnknownMethod" requested_reply="0" destination=":1.1756" (uid=0 pid=32345 comm="/usr/lib/bluetooth/bluetoothd ")
 4月 30 00:53:15 localhost dbus-daemon[426]: [system] Rejected send message, 1 matched rules; type="error", sender=":1.283" (uid=1000 pid=1520 comm="/usr/bin/pulseaudio --daemonize=no ") interface="(unset)" member="(unset)" error name="org.freedesktop.DBus.Error.UnknownMethod" requested_reply="0" destination=":1.1756" (uid=0 pid=32345 comm="/usr/lib/bluetooth/bluetoothd ")

```

## network?

```
 4月 29 18:43:24 localhost wpa_supplicant[907]: nl80211: Failed to set IPv4 unicast in multicast filter
```

