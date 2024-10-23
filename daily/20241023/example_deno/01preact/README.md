# preactのrender to stringをdenoから呼ぶ

- https://github.com/preactjs/preact-render-to-string?tab=readme-ov-file#render-preact-components-to-html

## setup

```console
$ deno init
$ deno add npm:preact
$ deno add npm:preact-render-to-string
```

## render to string

これで良いんだろうか？

```console
$ deno install
$ deno run main.tsx
<div class="box box-open"><span class="fox">Finn</span></div>
```

