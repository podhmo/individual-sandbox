# denoでgeminiのweb APIをライブラリを使わず呼んでみる

## 00 hello world

認証はkeyで済むみたい

```
  curl "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash:generateContent?key=${GEMINI_API_KEY}" \
    -H 'Content-Type: application/json' \
    -X POST \
    -d '{
      "contents": [{
        "parts":[{"text": "Write a story about a magic backpack."}]
        }]
       }'
```

それっぽい感じで作った。`gemini-1.5-flash` 以外のpathにアクセスしたい。

## references
- https://ai.google.dev/gemini-api/docs/api-key?hl=ja
- https://ai.google.dev/gemini-api/docs/text-generation?hl=ja&lang=rest
- https://aistudio.google.com/app/