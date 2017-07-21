## systemctl

listing all services

```
systemctl list-unit-files
```

checking status of services

```
systemctl status <service>
```

enabling services

```
systemctl enable <service>
```

- https://www.digitalocean.com/community/tutorials/how-to-use-systemctl-to-manage-systemd-services-and-units

## activate networkmanager

- networkmanager
- https://wiki.archlinux.org/index.php/NetworkManager

```
$ systemctl status NetworkManager.service
● NetworkManager.service - Network Manager
   Loaded: loaded (/usr/lib/systemd/system/NetworkManager.service; disabled; vendor preset: disabled)
   Active: inactive (dead)
     Docs: man:NetworkManager(8)
$ sudo systemctl enable NetworkManager.service
$ sudo systemctl start NetworkManager.service
$ sudo nmcli dev wifi connect <essid> password <password>
```


