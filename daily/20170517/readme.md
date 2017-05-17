# make 再帰的なmake

`$(MAKE)` と書くのが流儀っぽい。
ちなみに `-j` などが引き継がれるのかというと、MFLAGSあるいはMAKEFLAGSという環境変数により引き継がれるっぽい。

```make
run: echo.mk
	$(MAKE) -f echo.mk

clean:
	rm -f *.mk

echo.mk: echo.mk.j2
	kamidana $< --data data.yaml > $@
```
