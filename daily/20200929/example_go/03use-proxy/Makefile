name := myself

$(name).crt: $(name).key $(name).csr
	openssl x509 -days 3650 -req -signkey $(word 1,$^) < $(word 2,$^) > $@
$(name).csr: $(name).key
	openssl req -new -key $< > $@
$(name).key:
	openssl genrsa 2048 > $@
