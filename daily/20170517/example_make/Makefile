run: echo.mk
	$(MAKE) -f echo.mk

clean:
	rm -f *.mk

echo.mk: echo.mk.j2
	kamidana $< --data data.yaml > $@
