## networking tracing loop

hmm. tracing loop?

```
$ traceepath -b pod.hatena.com
 1?: [LOCALHOST]                      pmtu 1500
 1:  gateway (192.168.0.1)                                 2.048ms
 1:  gateway (192.168.0.1)                                 1.922ms
 2:  KHP059134003129.ppp-bb.dion.ne.jp (59.134.3.129)      4.070ms
 3:  oymBBAR001-2.bb.kddi.ne.jp (182.248.162.170)         13.475ms
 4:  tm4BBAC02.bb.kddi.ne.jp (118.152.213.62)             20.289ms asymm  6
 5:  otejbb206.int-gw.kddi.ne.jp (118.152.254.65)         16.218ms asymm  6
 6:  jc-ote301.int-gw.kddi.ne.jp (118.155.197.42)         16.791ms
 7:  124.211.10.42 (124.211.10.42)                        15.717ms
 8:  tkwrt1s-ort2.bb.sakura.ad.jp (157.17.130.26)         16.804ms
 9:  tkdrt1b-wrt1s.bb.sakura.ad.jp (157.17.130.206)       40.390ms
10:  tkdrt8c-drt1b.bb.sakura.ad.jp (59.106.247.146)       16.318ms
11:  59.106.81.178 (59.106.81.178)                        22.010ms
12:  no reply
13:  no reply
14:  no reply
15:  no reply
16:  no reply
17:  no reply
18:  no reply
19:  no reply
20:  no reply
21:  no reply
22:  no reply
23:  no reply
24:  no reply
25:  no reply
26:  no reply
27:  no reply
28:  no reply
29:  no reply
30:  no reply
     Too many hops: pmtu 1500
     Resume: pmtu 1500
```

`ICMP request` is filtered?.

## arch using dig

```
sudo pacman -S traceroute
sudo pacman -S dnsutils
```

```
$ dig pod.hatena.com +short
59.106.194.43
$ dig pod.hatena.com +noall +answer
; <<>> DiG 9.11.2 <<>> pod.hatena.com +noall +answer
;; global options: +cmd
pod.hatena.com.		295	IN	A	59.106.194.43
```

reverse

```
$ dig -x 8.8.8.8 +noall +answer

; <<>> DiG 9.11.2 <<>> -x 8.8.8.8 +noall +answer
;; global options: +cmd
8.8.8.8.in-addr.arpa.	78047	IN	PTR	google-public-dns-a.google.com.

$ dig -x 59.106.194.43 +noall +answer

; <<>> DiG 9.11.2 <<>> -x 59.106.194.43 +noall +answer
;; global options: +cmd
```

hmmm

```
$dig pod.hatena.com +noall +trace +answer
; <<>> DiG 9.11.2 <<>> pod.hatena.com +noall +trace +answer
;; global options: +cmd
.			185757	IN	NS	k.root-servers.net.
.			185757	IN	NS	l.root-servers.net.
.			185757	IN	NS	m.root-servers.net.
.			185757	IN	NS	a.root-servers.net.
.			185757	IN	NS	b.root-servers.net.
.			185757	IN	NS	c.root-servers.net.
.			185757	IN	NS	d.root-servers.net.
.			185757	IN	NS	e.root-servers.net.
.			185757	IN	NS	f.root-servers.net.
.			185757	IN	NS	g.root-servers.net.
.			185757	IN	NS	h.root-servers.net.
.			185757	IN	NS	i.root-servers.net.
.			185757	IN	NS	j.root-servers.net.
;; Received 811 bytes from 192.168.0.1#53(192.168.0.1) in 22 ms

com.			172800	IN	NS	l.gtld-servers.net.
com.			172800	IN	NS	h.gtld-servers.net.
com.			172800	IN	NS	a.gtld-servers.net.
com.			172800	IN	NS	b.gtld-servers.net.
com.			172800	IN	NS	j.gtld-servers.net.
com.			172800	IN	NS	m.gtld-servers.net.
com.			172800	IN	NS	g.gtld-servers.net.
com.			172800	IN	NS	k.gtld-servers.net.
com.			172800	IN	NS	f.gtld-servers.net.
ncom.			172800	IN	NS	c.gtld-servers.net.
com.			172800	IN	NS	e.gtld-servers.net.
com.			172800	IN	NS	i.gtld-servers.net.
com.			172800	IN	NS	d.gtld-servers.net.
com.			86400	IN	DS	30909 8 2 E2D3C916F6DEEAC73294E8268FB5885044A833FC5459588F4A9184CF C41A5766
com.			86400	IN	RRSIG	DS 8 1 86400 20170823170000 20170810160000 15768 . etb99JByUr++q1fjUh6j2dcV5llev9GImMhFlKhfdz6edRPWllTlkVoG 8d7CjdSTjF0YYTWHExStUlv5BUIVpIggBNMSfIlCK+dKh5OzQyx8VHN1 q155uv2ONnL35sSuE42kCb/WbpeaEaqX3TB4WwlnEgChGXkQp7S6074V TGIutD1C1PeeMp4PbTEmzlPVrpNATK5HqxMeJzmv9tEFVXwZK7Qqw/gD vHhsRJPzYMj6W4j+pu/zeiKNuOA9uY3RtLzqBX/CrPLELMVKK5suQbHH ucJ79GNKb5grvP44lVq8h1epOZsqk4QcG2iOYmFAJ3/fdIblrNSMtV/V tJ22oA==
;; Received 1174 bytes from 2001:500:2::c#53(c.root-servers.net) in 238 ms

hatena.com.		172800	IN	NS	ns0.future-s.com.
hatena.com.		172800	IN	NS	ns1.future-s.com.
CK0POJMG874LJREF7EFN8430QVIT8BSM.com. 86400 IN NSEC3 1 1 0 - CK0Q1GIN43N1ARRC9OSM6QPQR81H5M9A  NS SOA RRSIG DNSKEY NSEC3PARAM
CK0POJMG874LJREF7EFN8430QVIT8BSM.com. 86400 IN RRSIG NSEC3 8 2 86400 20170817044812 20170810033812 5528 com. ht2Mr91pZN/9RXxIXxbONdlD/q+daP1W7KzDzm1vlq8CbCo0QM2Stw4h SB8mmcmlK+EmtDj4+9duhcchID7WQTmgWHppuqAyZvTe6qF9/ayD++hg nuq9ZlmOXaPKeJSsY9UZqsVwjh4LmoJ8XEuO2Wp9YuC9wxqWeAE8e+kB 29c=
VK667QQM69QVTESVUI2C8GAP5NHQTOAN.com. 86400 IN NSEC3 1 1 0 - VK6719O4PK0F2DH6B601U6EC7HLFK9O3  NS DS RRSIG
VK667QQM69QVTESVUI2C8GAP5NHQTOAN.com. 86400 IN RRSIG NSEC3 8 2 86400 20170816041729 20170809030729 5528 com. d0Izej5Q/5ZQF0npmLHgSSBBD17ZeobgfB/KSXASI9NvKAy3XqzhGXvG c254j3DPieljpjmM3yRq8z8BZaC0zrmyOGZY7fZYrlP9y6QS69MeenDI +MMJq3krHl1+Of06kKkvqhnTTRJefspIvLzK0vBId69h0En6D3bXcf+D DhM=
;; Received 605 bytes from 192.26.92.30#53(c.gtld-servers.net) in 23 ms

pod.hatena.com.		600	IN	A	59.106.194.43
;; Received 48 bytes from 219.99.167.170#53(ns0.future-s.com) in 26 ms
```

hmm?

```
$ cat /etc/resolve.conf
cat /etc/resolv.conf
# Generated by resolvconf
nameserver 192.168.0.1
nameserver 2001:268:fd07:4::1
nameserver 2001:268:fd08:4::1
```

hmm.

```
$ for i in `seq 10`; do dig pod.hatena.com +noall +trace +answer > $i.txt; done
## too slow...
$ grep -r from .
./10.txt:;; Received 811 bytes from 192.168.0.1#53(192.168.0.1) in 20 ms
./10.txt:;; Received 1174 bytes from 2001:500:200::b#53(b.root-servers.net) in 174 ms
./10.txt:;; Received 605 bytes from 192.43.172.30#53(i.gtld-servers.net) in 67 ms
./10.txt:;; Received 48 bytes from 219.99.160.181#53(ns1.future-s.com) in 21 ms
./9.txt:;; Received 811 bytes from 192.168.0.1#53(192.168.0.1) in 21 ms
./9.txt:;; Received 1174 bytes from 192.112.36.4#53(g.root-servers.net) in 42 ms
./9.txt:;; Received 605 bytes from 2001:502:8cc::30#53(h.gtld-servers.net) in 157 ms
./9.txt:;; Received 48 bytes from 219.99.167.170#53(ns0.future-s.com) in 22 ms
./8.txt:;; Received 811 bytes from 192.168.0.1#53(192.168.0.1) in 17 ms
./8.txt:;; Received 1174 bytes from 2001:500:a8::e#53(e.root-servers.net) in 95 ms
./8.txt:;; Received 605 bytes from 192.48.79.30#53(j.gtld-servers.net) in 67 ms
./8.txt:;; Received 48 bytes from 219.99.167.170#53(ns0.future-s.com) in 22 ms
./7.txt:;; Received 811 bytes from 192.168.0.1#53(192.168.0.1) in 16 ms
./7.txt:;; Received 1174 bytes from 192.203.230.10#53(e.root-servers.net) in 94 ms
./7.txt:;; Received 605 bytes from 192.31.80.30#53(d.gtld-servers.net) in 17 ms
./7.txt:;; Received 48 bytes from 219.99.160.181#53(ns1.future-s.com) in 21 ms
./6.txt:;; Received 811 bytes from 192.168.0.1#53(192.168.0.1) in 19 ms
./6.txt:;; Received 1174 bytes from 2001:500:200::b#53(b.root-servers.net) in 206 ms
./6.txt:;; Received 605 bytes from 192.48.79.30#53(j.gtld-servers.net) in 68 ms
./6.txt:;; Received 48 bytes from 219.99.160.181#53(ns1.future-s.com) in 22 ms
./5.txt:;; Received 811 bytes from 192.168.0.1#53(192.168.0.1) in 18 ms
./5.txt:;; Received 1174 bytes from 2001:500:200::b#53(b.root-servers.net) in 138 ms
./5.txt:;; Received 605 bytes from 192.54.112.30#53(h.gtld-servers.net) in 68 ms
./5.txt:;; Received 48 bytes from 219.99.167.170#53(ns0.future-s.com) in 22 ms
./4.txt:;; Received 811 bytes from 192.168.0.1#53(192.168.0.1) in 19 ms
./4.txt:;; Received 1174 bytes from 2001:500:a8::e#53(e.root-servers.net) in 95 ms
./4.txt:;; Received 605 bytes from 2001:503:a83e::2:30#53(a.gtld-servers.net) in 192 ms
./4.txt:;; Received 48 bytes from 219.99.167.170#53(ns0.future-s.com) in 23 ms
./3.txt:;; Received 811 bytes from 2001:268:fd07:4::1#53(2001:268:fd07:4::1) in 17 ms
./3.txt:;; Received 1174 bytes from 2001:7fe::53#53(i.root-servers.net) in 331 ms
./3.txt:;; Received 605 bytes from 2001:501:b1f9::30#53(m.gtld-servers.net) in 204 ms
./3.txt:;; Received 48 bytes from 219.99.167.170#53(ns0.future-s.com) in 23 ms
./2.txt:;; Received 811 bytes from 2001:268:fd07:4::1#53(2001:268:fd07:4::1) in 19 ms
./1.txt:;; Received 811 bytes from 2001:268:fd07:4::1#53(2001:268:fd07:4::1) in 18 ms
./1.txt:;; Received 1202 bytes from 2001:7fd::1#53(k.root-servers.net) in 315 ms
```

hmm

```
$ ip route
default via 192.168.0.1 dev wlp2s0 proto static metric 600 
127.0.0.0/8 dev lo proto kernel scope host src 127.0.0.1 metric 201 
192.168.0.0/24 dev wlp2s0 proto kernel scope link src 192.168.0.7 
192.168.0.0/24 dev wlp2s0 proto kernel scope link src 192.168.0.7 metric 600 
```

```
ip neigh
192.168.0.1 dev wlp2s0 lladdr 1c:b1:7f:ad:d9:f4 REACHABLE
fe80::1eb1:7fff:fead:d9f4 dev wlp2s0 lladdr 1c:b1:7f:ad:d9:f4 router REACHABLE
240f:83:c3ee:1:1eb1:7fff:fead:d9f4 dev wlp2s0 lladdr 1c:b1:7f:ad:d9:f4 router STALE
```
