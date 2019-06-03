default: 00 01

# shape
00: src := 00similar
00: dst := 01shape
00:
	mkdir -p ${dst}
	for i in ${src}/*.yaml; do jqfpy 'list(get("definitions").values())[0]' $$i | dictknife shape --with-type | sort | tee ${dst}/$$(basename $${i%.yaml}.txt); done

# diff
01: src := 01shape
01: dst := 02diff
01: base := 01shape/HeaderWithSchemaWithExample.txt
01:
	mkdir -p ${dst}
	for i in ${src}/*.txt; do diff -u ${base} $$i | tee ${dst}/$$(basename $${i%.txt}.diff);  done
