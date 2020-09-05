## mac ifconfigのそれぞれの意味

- https://superuser.com/questions/267660/can-someone-please-explain-ifconfig-output-in-mac-os-x

```
lo0 = loopback
gif0 = Software Network Interface
stf0 = 6to4 tunnel interface
en0 = Ethernet 0
fw0 = Firewire
en1 = Ethernet 1
vmnet8 = Virtual Interface
vmnet1 = Virtual Interface
```

以下のコマンドでどういうものとして認識されているかもわかる。

```console
$ networksetup -listallhardwareports

Hardware Port: Wi-Fi
Device: en0
Ethernet Address: 8c:85:90:76:15:5b

Hardware Port: Bluetooth PAN
Device: en3
Ethernet Address: 8c:85:90:6a:bd:ab

Hardware Port: Thunderbolt 1
Device: en1
Ethernet Address: 82:fa:46:a3:04:01

Hardware Port: Thunderbolt 2
Device: en2
Ethernet Address: 82:fa:46:a3:04:00

Hardware Port: Thunderbolt Bridge
Device: bridge0
Ethernet Address: 82:fa:46:a3:04:01

VLAN Configurations
===================
```

あるいは `/Library/Preferences/SystemConfiguration/NetworkInterfaces.plist` などに設定が書かれている。
