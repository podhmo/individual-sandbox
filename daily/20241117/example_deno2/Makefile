build: dst_main0.ts dst_main1.ts dst_main2.mjs dst_main3.mjs dst_main4.mjs dst_main5.mjs

# 直接 jsr:<package> でimportしたファイル
dst_main0.ts: src_main0.ts tools.ts
	deno run -A tools.ts --dst dst --src $<
# deno.jsonのimportsを見てimportしたファイル
dst_main1.ts: src_main1.ts tools.ts
	deno run -A tools.ts --dst dst --src $<

# esbuildでのbundleを試す用のファイル
dst_main2.mjs: src_main2.ts tools2.ts
	deno run -A tools2.ts --dst $@ --src $<

# esbuildでのbundleを試す用のファイル。相対パスでimportしている。
dst_main3.mjs: src_main3.ts tools2.ts
	deno run -A tools2.ts --dst $@ --src $<

# deno.jsonのimportsを見てimportしたファイル
dst_main4.mjs: src_main4.ts tools2.ts
	deno run -A tools2.ts --dst $@ --src $< --config=deno.json --debug

# preactのcomponentを作ってそれを実行してみる
dst_main5.mjs: src_main5.tsx tools2.ts
	deno run -A tools2.ts --dst $@ --src $< --config=deno.json --debug
	deno run -A run5.mjs

clean:
	rm -rf dst dst_*
.PHONY: clean