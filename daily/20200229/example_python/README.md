さすがにevent loopの中でもrun_in_executor()で実行したほうが良い模様。

## 00

```console
$ DEBUG=1 LOGGING_TIME=relative python 00update_check.py
** handofcats.customize: DEBUG=1, activate logging **
level:DEBUG	name:asyncioL59	relative:142.3470973968506	message:Using selector: KqueueSelector
level:INFO	name:__main__L29	relative:142.9917812347412	message:do something (main)
level:INFO	name:__main__L31	relative:143.12076568603516	message:.
level:INFO	name:__main__L12	relative:143.30291748046875	message:-> update_check ...
level:INFO	name:__main__L31	relative:245.09906768798828	message:.
level:INFO	name:__main__L31	relative:350.3539562225342	message:.
level:INFO	name:__main__L31	relative:450.8779048919678	message:.
level:INFO	name:__main__L31	relative:556.2450885772705	message:.
level:INFO	name:__main__L14	relative:649.0440368652344	message:<- ... update_check
level:INFO	name:__main__L31	relative:657.6650142669678	message:.
level:INFO	name:__main__L34	relative:759.6948146820068	message:ok
level:INFO	name:__main__L38	relative:759.8278522491455	message:A new release of gh is available: xxx → 0.8.8
```

## 01

```console
$ DEBUG=1 LOGGING_TIME=relative python 01update_check.py
** handofcats.customize: DEBUG=1, activate logging **
level:DEBUG	name:asyncioL59	relative:126.30009651184082	message:Using selector: KqueueSelector
level:INFO	name:__main__L27	relative:126.98698043823242	message:do something (main)
level:INFO	name:__main__L29	relative:127.13217735290527	message:.
level:INFO	name:__main__L29	relative:230.23104667663574	message:.
level:INFO	name:__main__L29	relative:331.4962387084961	message:.
level:INFO	name:__main__L29	relative:436.73110008239746	message:.
level:INFO	name:__main__L29	relative:541.2042140960693	message:.
level:INFO	name:__main__L29	relative:643.9270973205566	message:.
level:INFO	name:__main__L31	relative:744.4562911987305	message:ok
level:INFO	name:__main__L12	relative:744.6391582489014	message:-> update_check ...
level:INFO	name:__main__L14	relative:1247.0462322235107	message:<- ... update_check
level:INFO	name:__main__L41	relative:1247.4212646484375	message:A new release of gh is available: xxx → 0.8.8
```

## 02

```console
DEBUG=1 LOGGING_TIME=relative python 02update_check.py
** handofcats.customize: DEBUG=1, activate logging **
level:DEBUG	name:asyncioL59	relative:111.0079288482666	message:Using selector: KqueueSelector
level:INFO	name:__main__L27	relative:114.61687088012695	message:do something (main)
level:INFO	name:__main__L29	relative:114.78495597839355	message:.
level:INFO	name:__main__L12	relative:115.04507064819336	message:-> update_check ...
level:INFO	name:__main__L29	relative:219.0859317779541	message:.
level:INFO	name:__main__L29	relative:319.61512565612793	message:.
level:INFO	name:__main__L29	relative:423.0461120605469	message:.
level:INFO	name:__main__L29	relative:526.7190933227539	message:.
level:INFO	name:__main__L14	relative:615.5180931091309	message:<- ... update_check
level:INFO	name:__main__L29	relative:632.1959495544434	message:.
level:INFO	name:__main__L31	relative:737.7939224243164	message:ok
level:INFO	name:__main__L42	relative:738.7328147888184	message:A new release of gh is available: xxx → 0.8.8
```
