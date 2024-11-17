build: dst/main0.ts

dst/main0.ts: src_main0.ts
	deno run -A tools.ts --dst dst --src $<

clean:
	rm -rf dst dst_*
.PHONY: clean