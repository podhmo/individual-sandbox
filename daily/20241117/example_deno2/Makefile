build: dst_main0.ts dst_main1.ts dst_main2.ts

# 直接 jsr:<package> でimportしたファイル
dst_main0.ts: src_main0.ts tools.ts
	deno run -A tools.ts --dst dst --src $<
# deno.jsonのimportsを見てimportしたファイル
dst_main1.ts: src_main1.ts tools.ts
	deno run -A tools.ts --dst dst --src $<

# esbuildでのbundleを試す用のファイル
dst_main2.ts: src_main2.ts tools2.ts
	deno run -A tools2.ts --dst dst --src $<

clean:
	rm -rf dst dst_*
.PHONY: clean