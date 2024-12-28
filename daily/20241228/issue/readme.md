以下の文章を英語に翻訳してください。文脈はossでのissue作成です。

件名:特定のURLをキャッシュしようとしたときに TypeError: file name contained an unexpected NUL byte: lstat が発生する。

詳細：

これはなかなか面白いエラーでした。ある特定のURLをキャッシュしようとしたときにだけ以下のようなエラーが発生していました。

```
Uncaught (in promise) TypeError: file name contained an unexpected NUL byte: lstat '<chache dir>/�g��E=m�Y:M���t ���-�Ƀ�c���j'
```

再現する最小限のコードは以下です。

```ts
import { cache } from "jsr:@denosaurs/cache@0.2.15";

const ns = "";
const url = "https://esm.sh/stable/react@18";
const fs = await cache(url, undefined, ns);
console.log("cached: %o %o", fs.path, fs.meta);
```

唯一esm.shで`react@18`を与えたときにだけ発生したのでたまたま確率的に遭遇したエラーだとおもいます。私は興味深く感じたので少し調べてみました。
どうやら、キャッシュを保存するときのファイル名を計算する処理をでのSHA256と入力の組みあわせによってはヌル文字が含まれてしまうようでした。

より依存を減らしたコードは以下のようになります。

```ts
// pick from https://github.com/denosaurs/cache/blob/83e4287694b0affe39bcad72f4cb7f99d3679057/file.ts#L125
async function hash(url: URL): Promise<string> {
    const formatted = `${url.pathname}${url.search ? "?" + url.search : ""}`;
    const encoder = new TextEncoder();
    const data = encoder.encode(formatted);
    const hash = await crypto.subtle.digest("SHA-256", data);
    return new TextDecoder().decode(hash);
}

// await Deno.lstat("hello\0world"); // same error is occured

const url = "https://esm.sh/stable/react@18";
const calculatedPath = await hash(new URL(url));
console.log("calculated path", calculatedPath);
console.log("has null byte?", calculatedPath.includes("\0")); // true
console.log(await Deno.lstat(calculatedPath)); // same error is occured
```

