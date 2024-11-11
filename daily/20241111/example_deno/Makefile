YOUTUBE_API_KEY ?= $(YOUTUBE_API_KEY)

run:
	YOUTUBE_API_KEY=$(YOUTUBE_API_KEY) deno run -A fetch.ts --order viewCount --query="ゆっくり AND 料理" | tee result.json
.PHONY: run