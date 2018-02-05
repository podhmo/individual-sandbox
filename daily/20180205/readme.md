## arch disable ipv6

/etc/sysctl.d/40-ipv6.conf

```
net.ipv6.conf.all.disable_ipv6 = 1
net.ipv6.conf.default.disable_ipv6 = 1
net.ipv6.conf.docker0.disable_ipv6 = 1
net.ipv6.conf.wlp2s0.disable_ipv6 = 1
```

see

- [IPv6 - ArchWiki](https://wiki.archlinux.org/index.php/IPv6#Disable_IPv6 "IPv6 - ArchWiki")
