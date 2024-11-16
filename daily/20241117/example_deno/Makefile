build: chunks.dist.ts with-help.dist.js

# make -B build

# 簡単なバンドルを試す
chunks.dist.ts: chunks.ts bundle.ts
	deno run -A bundle.ts  --src $< --dst $@

# 他のプロジェクトの設定を使う
with-help.dist.js: ~/ghq/github.com/podhmo/with-help/mod.ts bundle.ts
	deno run -A bundle.ts  --src $< --dst $@ --config ~/ghq/github.com/podhmo/with-help/deno.json