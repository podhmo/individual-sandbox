DB ?= data.db
OPTS = -echo

default: clean $(DB) 00 01 02 03 04


$(DB): insert.sql
	cat $< | sqlite3 $(OPTS) $@

00: $(DB)
	cat $(shell echo $@*.sql) | sqlite3 $(OPTS) $(DB)
01: $(DB)
	cat $(shell echo $@*.sql) | sqlite3 $(OPTS) $(DB)
02: $(DB)
	cat $(shell echo $@*.sql) | sqlite3 $(OPTS) $(DB)
03: $(DB)
	cat $(shell echo $@*.sql) | sqlite3 $(OPTS) $(DB)
04: $(DB)
	cat $(shell echo $@*.sql) | sqlite3 $(OPTS) $(DB)

clean:
	rm -f $(DB)
.PHONY: clean
