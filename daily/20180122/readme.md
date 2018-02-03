## arch vpn (L2TP)

```
# pacaur -Ss l2tp
pacaur -Sy l2tp-ipsec-vpn
```

strongswanとopenswanがあるらしい。

- https://serverfault.com/questions/798635/ipsec-for-linux-strongswan-vs-openswan-vs-libreswan-vs-other

networkmanagerで使うのが一番楽そう

```
$ sudo pacman -Sy xl2tpd strongswan networkmanager-strongswan strongswan
$ nmcli dev status
# 動かしていないとだめ
$ systemctl start systemd-networkd.service 
```
networkmanager-strongswanで設定できるの IKEv2だった。使いたいのはL2TP

- https://thebestvpn.com/pptp-l2tp-openvpn-sstp-ikev2-protocols/

```
$ pacaur -Sy networkmanager-libreswan
```

libreswanのbuildに失敗する

読み返さないとだめっぽい。

https://wiki.archlinux.jp/index.php/Arch_User_Repository

```
$ sudo pacman -R networkmanager-strongswan strongswan
$ cd ~/.cache/pacaur/libreswan
$ vim PKGBUILD
$ makepkg -si # -s 依存解決, -i install
```

```diff
diff --git a/PKGBUILD b/PKGBUILD
index 1796f94..2da2489 100644
--- a/PKGBUILD
+++ b/PKGBUILD
@@ -9,8 +9,8 @@ pkgdesc="IPsec implementation with IKEv1 and IKEv2 keying protocols"
 arch=('i686' 'x86_64')
 url="https://libreswan.org/"
 license=('GPL' 'MPL')
-depends=('systemd' 'unbound' 'nss' 'libcap-ng' 'curl')
-makedepends=('docbook-xsl' 'xmlto' 'flex' 'bison')
+depends=('systemd' 'unbound' 'nss' 'libcap-ng' 'curl' 'docbook-xsl' 'xmlto')
+makedepends=('flex' 'bison')
 conflicts=('freeswan' 'openswan' 'strongswan' 'ipsec-tools')
 backup=('etc/ipsec.conf' 'etc/ipsec.secrets' 'etc/pam.d/pluto')
 source=(https://download.libreswan.org/${pkgname}-${pkgver}.tar.gz
```

## gpg

```
pacaur networkmanager-l2tp
==> Verifying source file signatures with gpg...
    network-manager-l2tp-1.2.8.tar.gz ... FAILED (unknown public key 49A7787EF8D3C039)
==> ERROR: One or more PGP signatures could not be verified!
```

cat PKGBUILD

```
validpgpkeys=('E48BD89A1C51BFA28413D18349A7787EF8D3C039') # Douglas Kosovic
```

http://allanmcrae.com/2015/01/two-pgp-keyrings-for-package-management-in-arch-linux/

GPG持っていなさそう

```
gpg --list-keys
gpg --recv-keys <key id>
gpg --lsign <key id>  # once
```

https://www.clvs7.com/blog/2015/11/23/l2tp-ipsec-arch/


```
/etc/ipsec.d/cacerts
```

## journalctl

```
journalctl -exf
```


## libreswan

```
]$ systemctl list-unit-files  | grep ip
ip6tables.service                           disabled       
ipsec.service                               disabled       
iptables.service                            disabled  
```

```
sudo systemctl enable ipsec.service
```

## ike-scan

pacaur -Sy ike-scan
ike-scan vpn.wacul-ai.com

https://askubuntu.com/questions/904217/unable-to-connect-l2tp-ipsec-vpn-from-ubuntu-16-04

```
Starting ike-scan 1.9 with 1 hosts (http://www.nta-monitor.com/tools/ike-scan/)
39.110.205.108  Main Mode Handshake returned HDR=(CKY-R=f1b565225d8bd2e5) SA=(Enc=3DES Hash=SHA1 Auth=PSK Group=2:modp1024 LifeType=Seconds LifeDuration=28800) VID=4a131c81070358455c5728f20e95452f (RFC 3947 NAT-T) VID=7d9419a65310ca6f2c179d9215529d56 (draft-ietf-ipsec-nat-t-ike-03) VID=90cb80913ebb696e086381b5ec427b1f (draft-ietf-ipsec-nat-t-ike-02\n) VID=cd60464335df21f87cfdb2fc68b6a448 (draft-ietf-ipsec-nat-t-ike-02) VID=4485152d18b6bbcd0be8a8469579ddcc (draft-ietf-ipsec-nat-t-ike-00) VID=afcad71368a1f1c96b8696fc77570100 (Dead Peer Detection v1.0)
```

```
    Phase1 Algorithms : 3des-sha1-modp1024

    Phase2 Algorithms : 3des-sha1
```
