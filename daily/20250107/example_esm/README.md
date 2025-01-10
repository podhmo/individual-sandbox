# esm.shをローカルで動かす

昨日調べたときに謎のエラーに遭遇した。原因がわからないのでやりづらい。
(実際のところは謎のエラーではなくdepsを大量に書いた場合のエラー)

- https://gist.github.com/podhmo/395689b310af88566f1df31ed218592d#file-readme-md

## 00 setup

https://github.com/esm-dev/esm.sh/blob/main/HOSTING.md

このへんを観た感じgoが入っていれば動きそう。

```console
$ ghq get -p esm-dev/esm.sh
$ cd ~/ghq/github.com/esm-dev/esm.sh
$ go run main.go --config=config.json
```

config.json

```json
{
  "port": 44444,
  "npmRegistry": "https://registry.npmjs.org/",
  "npmToken": "******"
}
```

## 01 run

なぜかDownloadから先に進まない。めんどくさいのでvoltaでinstallする。

```console
$ go build -o esm.sh -v .
$ ./esm.sh --config config.json
Downloading https://nodejs.org/dist/v22.12.0/node-v22.12.0-linux-x64.tar.xz ...
2025/01/07 06:04:20 [fatal] nodejs: exit status 2: tar (child): node-v22.12.0-linux-x64.tar.xz: Cannot open: No such file or directory
tar (child): Error is not recoverable: exiting now
tar: Child returned status 2
tar: Error is not recoverable: exiting now
```

### volta でnode.jsをインストールする

server/nodejs.go

```go
const (
	nodejsMinVersion = 22
	nodeTypesVersion = "22.9.0"
	pnpmMinVersion   = "9.0.0"
)
```

https://volta.sh/

```console
# sudo apt remove nodejs npm; rm -rf ~/.npm/
$ curl https://get.volta.sh | bash -ux
$ ~/.volta/bin/volta setup
$ volta install node@22.12.0
$ volta install pnpm@9
```

### 実際に試してみる

server側

```console
$ go build -o esm.sh -v .
$ esm.sh --config config  # port=44444
2025/01/07 06:13:41 [info] Server is ready on http://localhost:44444
2025/01/07 06:13:58 [info] build '/react@18.3.1/es2022/react.mjs' done in 2.81232928s
```

client側

```console
$ http :44444/react@18
/* esm.sh - react@18.3.1 */
export * from "/react@18.3.1/es2022/react.mjs";
export { default } from "/react@18.3.1/es2022/react.mjs";
```

保存先はこのあたり

```
$ tree ~/.esmd/
/home/po/.esmd/
├── log
│   ├── access-20250107.log
│   ├── server-20241122.log
│   └── server-20250107.log
├── npm
│   ├── @esm.sh
│   │   └── cjs-module-lexer@1.0.1
│   │       ├── cjs_module_lexer.js
│   │       ├── package.json
│   │       └── pnpm-lock.yaml
│   ├── @types
│   │   └── react@18.3.18
│   │       └── package.json
│   ├── react@18.3.1
│   │   └── package.json
│   └── unenv-nightly@2.0.0-20241111-080453-894aa31
│       ├── package.json
│       └── pnpm-lock.yaml
└── storage
    └── builds
        └── react@18.3.1
            └── es2022
                ├── react.mjs
                ├── react.mjs.map
                └── react.mjs.meta
```

## 02 失敗したリクエストを試す

https://gist.github.com/podhmo/395689b310af88566f1df31ed218592d#01-readme%E3%82%92api%E3%81%8B%E3%82%89%E5%8F%96%E3%81%A3%E3%81%A6%E3%81%8F%E3%82%8B

普通に動きそう

```console
$ http ":44444/react-markdown@8.0.7?deps=@types/debug@4.1.12,@types/hast@2.3.10,@types/mdast@3.0.15,@types/ms@0.7.34,@types/prop-types@15.7.14,@types/react@18.3.18,@types/unist@2.0.11,bail@2.0.2,character-entities@2.0.2,comma-separated-tokens@2.0.3,csstype@3.1.3,debug@4.4.0,decode-named-character-reference@1.0.2,dequal@2.0.3,diff@5.2.0,extend@3.0.2,hast-util-whitespace@2.0.1,inline-style-parser@0.1.1,is-buffer@2.0.5,is-plain-obj@4.1.0,js-tokens@4.0.0,kleur@4.1.5,loose-envify@1.4.0,mdast-util-definitions@5.1.2,mdast-util-from-markdown@1.3.1,mdast-util-to-hast@12.3.0,mdast-util-to-string@3.2.0,micromark-core-commonmark@1.1.0,micromark-factory-destination@1.1.0,micromark-factory-label@1.1.0,micromark-factory-space@1.1.0,micromark-factory-title@1.1.0,micromark-factory-whitespace@1.1.0,micromark-util-character@1.2.0,micromark-util-chunked@1.1.0,micromark-util-classify-character@1.1.0,micromark-util-combine-extensions@1.1.0,micromark-util-decode-numeric-character-reference@1.1.0,micromark-util-decode-string@1.1.0,micromark-util-encode@1.1.0,micromark-util-html-tag-name@1.2.0,micromark-util-normalize-identifier@1.1.0,micromark-util-resolve-all@1.1.0,micromark-util-sanitize-uri@1.2.0,micromark-util-subtokenize@1.1.0,micromark-util-symbol@1.1.0,micromark-util-types@1.1.0,micromark@3.2.0,mri@1.2.0,ms@2.1.3,object-assign@4.1.1,prop-types@15.8.1,property-information@6.5.0,react-is@16.13.1,react-is@18.3.1,react@18.3.1,remark-parse@10.0.2,remark-rehype@10.1.0,sade@1.8.1,space-separated-tokens@2.0.2,style-to-object@0.4.4,trim-lines@3.0.1,trough@2.2.0,unified@10.1.2,unist-util-generated@2.0.1,unist-util-is@5.2.1,unist-util-position@4.0.4,unist-util-stringify-position@3.0.3,unist-util-visit-parents@5.1.3,unist-util-visit@4.1.2,uvu@0.5.6,vfile-message@3.1.4,vfile@5.3.7"

HTTP/1.1 200 OK
Cache-Control: public, max-age=31536000, immutable
Connection: keep-alive
Content-Encoding: gzip
Content-Type: application/javascript; charset=utf-8
Date: Mon, 06 Jan 2025 21:20:12 GMT
Server: esm.sh
Transfer-Encoding: chunked
Vary: Origin, User-Agent, Accept-Encoding
X-Esm-Path: /react-markdown@8.0.7/X-ZGJhaWxAMi4wLjIsY2hhcmFjdGVyLWVudGl0aWVzQDIuMC4yLGNvbW1hLXNlcGFyYXRlZC10b2tlbnNAMi4wLjMsZGVidWdANC40LjAsZGVjb2RlLW5hbWVkLWNoYXJhY3Rlci1yZWZlcmVuY2VAMS4wLjIsZGVxdWFsQDIuMC4zLGRpZmZANS4yLjAsZXh0ZW5kQDMuMC4yLGhhc3QtdXRpbC13aGl0ZXNwYWNlQDIuMC4xLGlubGluZS1zdHlsZS1wYXJzZXJAMC4xLjEsanMtdG9rZW5zQDQuMC4wLGtsZXVyQDQuMS41LGxvb3NlLWVudmlmeUAxLjQuMCxtZGFzdC11dGlsLWRlZmluaXRpb25zQDUuMS4yLG1kYXN0LXV0aWwtZnJvbS1tYXJrZG93bkAxLjMuMSxtZGFzdC11dGlsLXRvLWhhc3RAMTIuMy4wLG1kYXN0LXV0aWwtdG8tc3RyaW5nQDMuMi4wLG1pY3JvbWFyay1jb3JlLWNvbW1vbm1hcmtAMS4xLjAsbWljcm9tYXJrLWZhY3RvcnktZGVzdGluYXRpb25AMS4xLjAsbWljcm9tYXJrLWZhY3RvcnktbGFiZWxAMS4xLjAsbWljcm9tYXJrLWZhY3Rvcnktc3BhY2VAMS4xLjAsbWljcm9tYXJrLWZhY3RvcnktdGl0bGVAMS4xLjAsbWljcm9tYXJrLWZhY3Rvcnktd2hpdGVzcGFjZUAxLjEuMCxtaWNyb21hcmstdXRpbC1jaGFyYWN0ZXJAMS4yLjAsbWljcm9tYXJrLXV0aWwtY2h1bmtlZEAxLjEuMCxtaWNyb21hcmstdXRpbC1jbGFzc2lmeS1jaGFyYWN0ZXJAMS4xLjAsbWljcm9tYXJrLXV0aWwtY29tYmluZS1leHRlbnNpb25zQDEuMS4wLG1pY3JvbWFyay11dGlsLWRlY29kZS1udW1lcmljLWNoYXJhY3Rlci1yZWZlcmVuY2VAMS4xLjAsbWljcm9tYXJrLXV0aWwtZGVjb2RlLXN0cmluZ0AxLjEuMCxtaWNyb21hcmstdXRpbC1lbmNvZGVAMS4xLjAsbWljcm9tYXJrLXV0aWwtaHRtbC10YWctbmFtZUAxLjIuMCxtaWNyb21hcmstdXRpbC1ub3JtYWxpemUtaWRlbnRpZmllckAxLjEuMCxtaWNyb21hcmstdXRpbC1yZXNvbHZlLWFsbEAxLjEuMCxtaWNyb21hcmstdXRpbC1zYW5pdGl6ZS11cmlAMS4yLjAsbWljcm9tYXJrLXV0aWwtc3VidG9rZW5pemVAMS4xLjAsbWljcm9tYXJrLXV0aWwtc3ltYm9sQDEuMS4wLG1pY3JvbWFyay11dGlsLXR5cGVzQDEuMS4wLG1pY3JvbWFya0AzLjIuMCxtcmlAMS4yLjAsbXNAMi4xLjMsb2JqZWN0LWFzc2lnbkA0LjEuMSxwcm9wLXR5cGVzQDE1LjguMSxwcm9wZXJ0eS1pbmZvcm1hdGlvbkA2LjUuMCxyZWFjdC1pc0AxOC4zLjEscmVhY3RAMTguMy4xLHJlbWFyay1wYXJzZUAxMC4wLjIscmVtYXJrLXJlaHlwZUAxMC4xLjAsc2FkZUAxLjguMSxzcGFjZS1zZXBhcmF0ZWQtdG9rZW5zQDIuMC4yLHN0eWxlLXRvLW9iamVjdEAwLjQuNCx0cmltLWxpbmVzQDMuMC4xLHRyb3VnaEAyLjIuMCx1bmlmaWVkQDEwLjEuMix1bmlzdC11dGlsLWdlbmVyYXRlZEAyLjAuMSx1bmlzdC11dGlsLWlzQDUuMi4xLHVuaXN0LXV0aWwtcG9zaXRpb25ANC4wLjQsdW5pc3QtdXRpbC1zdHJpbmdpZnktcG9zaXRpb25AMy4wLjMsdW5pc3QtdXRpbC12aXNpdC1wYXJlbnRzQDUuMS4zLHVuaXN0LXV0aWwtdmlzaXRANC4xLjIsdXZ1QDAuNS42LHZmaWxlLW1lc3NhZ2VAMy4xLjQsdmZpbGVANS4zLjc/es2022/react-markdown.mjs
X-Typescript-Types: http://localhost:44444/react-markdown@8.0.7/X-ZGJhaWxAMi4wLjIsY2hhcmFjdGVyLWVudGl0aWVzQDIuMC4yLGNvbW1hLXNlcGFyYXRlZC10b2tlbnNAMi4wLjMsZGVidWdANC40LjAsZGVjb2RlLW5hbWVkLWNoYXJhY3Rlci1yZWZlcmVuY2VAMS4wLjIsZGVxdWFsQDIuMC4zLGRpZmZANS4yLjAsZXh0ZW5kQDMuMC4yLGhhc3QtdXRpbC13aGl0ZXNwYWNlQDIuMC4xLGlubGluZS1zdHlsZS1wYXJzZXJAMC4xLjEsanMtdG9rZW5zQDQuMC4wLGtsZXVyQDQuMS41LGxvb3NlLWVudmlmeUAxLjQuMCxtZGFzdC11dGlsLWRlZmluaXRpb25zQDUuMS4yLG1kYXN0LXV0aWwtZnJvbS1tYXJrZG93bkAxLjMuMSxtZGFzdC11dGlsLXRvLWhhc3RAMTIuMy4wLG1kYXN0LXV0aWwtdG8tc3RyaW5nQDMuMi4wLG1pY3JvbWFyay1jb3JlLWNvbW1vbm1hcmtAMS4xLjAsbWljcm9tYXJrLWZhY3RvcnktZGVzdGluYXRpb25AMS4xLjAsbWljcm9tYXJrLWZhY3RvcnktbGFiZWxAMS4xLjAsbWljcm9tYXJrLWZhY3Rvcnktc3BhY2VAMS4xLjAsbWljcm9tYXJrLWZhY3RvcnktdGl0bGVAMS4xLjAsbWljcm9tYXJrLWZhY3Rvcnktd2hpdGVzcGFjZUAxLjEuMCxtaWNyb21hcmstdXRpbC1jaGFyYWN0ZXJAMS4yLjAsbWljcm9tYXJrLXV0aWwtY2h1bmtlZEAxLjEuMCxtaWNyb21hcmstdXRpbC1jbGFzc2lmeS1jaGFyYWN0ZXJAMS4xLjAsbWljcm9tYXJrLXV0aWwtY29tYmluZS1leHRlbnNpb25zQDEuMS4wLG1pY3JvbWFyay11dGlsLWRlY29kZS1udW1lcmljLWNoYXJhY3Rlci1yZWZlcmVuY2VAMS4xLjAsbWljcm9tYXJrLXV0aWwtZGVjb2RlLXN0cmluZ0AxLjEuMCxtaWNyb21hcmstdXRpbC1lbmNvZGVAMS4xLjAsbWljcm9tYXJrLXV0aWwtaHRtbC10YWctbmFtZUAxLjIuMCxtaWNyb21hcmstdXRpbC1ub3JtYWxpemUtaWRlbnRpZmllckAxLjEuMCxtaWNyb21hcmstdXRpbC1yZXNvbHZlLWFsbEAxLjEuMCxtaWNyb21hcmstdXRpbC1zYW5pdGl6ZS11cmlAMS4yLjAsbWljcm9tYXJrLXV0aWwtc3VidG9rZW5pemVAMS4xLjAsbWljcm9tYXJrLXV0aWwtc3ltYm9sQDEuMS4wLG1pY3JvbWFyay11dGlsLXR5cGVzQDEuMS4wLG1pY3JvbWFya0AzLjIuMCxtcmlAMS4yLjAsbXNAMi4xLjMsb2JqZWN0LWFzc2lnbkA0LjEuMSxwcm9wLXR5cGVzQDE1LjguMSxwcm9wZXJ0eS1pbmZvcm1hdGlvbkA2LjUuMCxyZWFjdC1pc0AxOC4zLjEscmVhY3RAMTguMy4xLHJlbWFyay1wYXJzZUAxMC4wLjIscmVtYXJrLXJlaHlwZUAxMC4xLjAsc2FkZUAxLjguMSxzcGFjZS1zZXBhcmF0ZWQtdG9rZW5zQDIuMC4yLHN0eWxlLXRvLW9iamVjdEAwLjQuNCx0cmltLWxpbmVzQDMuMC4xLHRyb3VnaEAyLjIuMCx1bmlmaWVkQDEwLjEuMix1bmlzdC11dGlsLWdlbmVyYXRlZEAyLjAuMSx1bmlzdC11dGlsLWlzQDUuMi4xLHVuaXN0LXV0aWwtcG9zaXRpb25ANC4wLjQsdW5pc3QtdXRpbC1zdHJpbmdpZnktcG9zaXRpb25AMy4wLjMsdW5pc3QtdXRpbC12aXNpdC1wYXJlbnRzQDUuMS4zLHVuaXN0LXV0aWwtdmlzaXRANC4xLjIsdXZ1QDAuNS42LHZmaWxlLW1lc3NhZ2VAMy4xLjQsdmZpbGVANS4zLjc/index.d.ts

/* esm.sh - react-markdown@8.0.7 */
import "/comma-separated-tokens@2.0.3/es2022/comma-separated-tokens.mjs";
import "/hast-util-whitespace@2.0.1/es2022/hast-util-whitespace.mjs";
import "/prop-types@15.8.1/X-ZGpzLXRva2Vuc0A0LjAuMCxsb29zZS1lbnZpZnlAMS40LjAsb2JqZWN0LWFzc2lnbkA0LjEuMSxyZWFjdC1pc0AxOC4zLjE/es2022/prop-types.mjs";
import "/property-information@6.5.0/es2022/property-information.mjs";
import "/react-is@18.3.1/es2022/react-is.mjs";
import "/react@18.3.1/es2022/react.mjs";
import "/remark-parse@10.0.2/X-ZGJhaWxAMi4wLjIsY2hhcmFjdGVyLWVudGl0aWVzQDIuMC4yLGRlYnVnQDQuNC4wLGRlY29kZS1uYW1lZC1jaGFyYWN0ZXItcmVmZXJlbmNlQDEuMC4yLGRlcXVhbEAyLjAuMyxkaWZmQDUuMi4wLGV4dGVuZEAzLjAuMixrbGV1ckA0LjEuNSxtZGFzdC11dGlsLWZyb20tbWFya2Rvd25AMS4zLjEsbWRhc3QtdXRpbC10by1zdHJpbmdAMy4yLjAsbWljcm9tYXJrLWNvcmUtY29tbW9ubWFya0AxLjEuMCxtaWNyb21hcmstZmFjdG9yeS1kZXN0aW5hdGlvbkAxLjEuMCxtaWNyb21hcmstZmFjdG9yeS1sYWJlbEAxLjEuMCxtaWNyb21hcmstZmFjdG9yeS1zcGFjZUAxLjEuMCxtaWNyb21hcmstZmFjdG9yeS10aXRsZUAxLjEuMCxtaWNyb21hcmstZmFjdG9yeS13aGl0ZXNwYWNlQDEuMS4wLG1pY3JvbWFyay11dGlsLWNoYXJhY3RlckAxLjIuMCxtaWNyb21hcmstdXRpbC1jaHVua2VkQDEuMS4wLG1pY3JvbWFyay11dGlsLWNsYXNzaWZ5LWNoYXJhY3RlckAxLjEuMCxtaWNyb21hcmstdXRpbC1jb21iaW5lLWV4dGVuc2lvbnNAMS4xLjAsbWljcm9tYXJrLXV0aWwtZGVjb2RlLW51bWVyaWMtY2hhcmFjdGVyLXJlZmVyZW5jZUAxLjEuMCxtaWNyb21hcmstdXRpbC1kZWNvZGUtc3RyaW5nQDEuMS4wLG1pY3JvbWFyay11dGlsLWVuY29kZUAxLjEuMCxtaWNyb21hcmstdXRpbC1odG1sLXRhZy1uYW1lQDEuMi4wLG1pY3JvbWFyay11dGlsLW5vcm1hbGl6ZS1pZGVudGlmaWVyQDEuMS4wLG1pY3JvbWFyay11dGlsLXJlc29sdmUtYWxsQDEuMS4wLG1pY3JvbWFyay11dGlsLXNhbml0aXplLXVyaUAxLjIuMCxtaWNyb21hcmstdXRpbC1zdWJ0b2tlbml6ZUAxLjEuMCxtaWNyb21hcmstdXRpbC1zeW1ib2xAMS4xLjAsbWljcm9tYXJrLXV0aWwtdHlwZXNAMS4xLjAsbWljcm9tYXJrQDMuMi4wLG1yaUAxLjIuMCxtc0AyLjEuMyxzYWRlQDEuOC4xLHRyb3VnaEAyLjIuMCx1bmlmaWVkQDEwLjEuMix1bmlzdC11dGlsLXN0cmluZ2lmeS1wb3NpdGlvbkAzLjAuMyx1dnVAMC41LjYsdmZpbGUtbWVzc2FnZUAzLjEuNCx2ZmlsZUA1LjMuNw/es2022/remark-parse.mjs";
import "/remark-rehype@10.1.0/X-ZGJhaWxAMi4wLjIsZXh0ZW5kQDMuMC4yLG1kYXN0LXV0aWwtZGVmaW5pdGlvbnNANS4xLjIsbWRhc3QtdXRpbC10by1oYXN0QDEyLjMuMCxtaWNyb21hcmstdXRpbC1jaGFyYWN0ZXJAMS4yLjAsbWljcm9tYXJrLXV0aWwtZW5jb2RlQDEuMS4wLG1pY3JvbWFyay11dGlsLXNhbml0aXplLXVyaUAxLjIuMCxtaWNyb21hcmstdXRpbC1zeW1ib2xAMS4xLjAsbWljcm9tYXJrLXV0aWwtdHlwZXNAMS4xLjAsdHJpbS1saW5lc0AzLjAuMSx0cm91Z2hAMi4yLjAsdW5pZmllZEAxMC4xLjIsdW5pc3QtdXRpbC1nZW5lcmF0ZWRAMi4wLjEsdW5pc3QtdXRpbC1pc0A1LjIuMSx1bmlzdC11dGlsLXBvc2l0aW9uQDQuMC40LHVuaXN0LXV0aWwtc3RyaW5naWZ5LXBvc2l0aW9uQDMuMC4zLHVuaXN0LXV0aWwtdmlzaXQtcGFyZW50c0A1LjEuMyx1bmlzdC11dGlsLXZpc2l0QDQuMS4yLHZmaWxlLW1lc3NhZ2VAMy4xLjQsdmZpbGVANS4zLjc/es2022/remark-rehype.mjs";
import "/space-separated-tokens@2.0.2/es2022/space-separated-tokens.mjs";
import "/style-to-object@0.4.4/X-ZGlubGluZS1zdHlsZS1wYXJzZXJAMC4xLjE/es2022/style-to-object.mjs";
import "/unified@10.1.2/X-ZGJhaWxAMi4wLjIsZXh0ZW5kQDMuMC4yLHRyb3VnaEAyLjIuMCx1bmlzdC11dGlsLXN0cmluZ2lmeS1wb3NpdGlvbkAzLjAuMyx2ZmlsZS1tZXNzYWdlQDMuMS40LHZmaWxlQDUuMy43/es2022/unified.mjs";
import "/unist-util-visit@4.1.2/X-ZHVuaXN0LXV0aWwtaXNANS4yLjEsdW5pc3QtdXRpbC12aXNpdC1wYXJlbnRzQDUuMS4z/es2022/unist-util-visit.mjs";
import "/vfile@5.3.7/X-ZHVuaXN0LXV0aWwtc3RyaW5naWZ5LXBvc2l0aW9uQDMuMC4zLHZmaWxlLW1lc3NhZ2VAMy4xLjQ/es2022/vfile.mjs";
export * from "/react-markdown@8.0.7/X-ZGJhaWxAMi4wLjIsY2hhcmFjdGVyLWVudGl0aWVzQDIuMC4yLGNvbW1hLXNlcGFyYXRlZC10b2tlbnNAMi4wLjMsZGVidWdANC40LjAsZGVjb2RlLW5hbWVkLWNoYXJhY3Rlci1yZWZlcmVuY2VAMS4wLjIsZGVxdWFsQDIuMC4zLGRpZmZANS4yLjAsZXh0ZW5kQDMuMC4yLGhhc3QtdXRpbC13aGl0ZXNwYWNlQDIuMC4xLGlubGluZS1zdHlsZS1wYXJzZXJAMC4xLjEsanMtdG9rZW5zQDQuMC4wLGtsZXVyQDQuMS41LGxvb3NlLWVudmlmeUAxLjQuMCxtZGFzdC11dGlsLWRlZmluaXRpb25zQDUuMS4yLG1kYXN0LXV0aWwtZnJvbS1tYXJrZG93bkAxLjMuMSxtZGFzdC11dGlsLXRvLWhhc3RAMTIuMy4wLG1kYXN0LXV0aWwtdG8tc3RyaW5nQDMuMi4wLG1pY3JvbWFyay1jb3JlLWNvbW1vbm1hcmtAMS4xLjAsbWljcm9tYXJrLWZhY3RvcnktZGVzdGluYXRpb25AMS4xLjAsbWljcm9tYXJrLWZhY3RvcnktbGFiZWxAMS4xLjAsbWljcm9tYXJrLWZhY3Rvcnktc3BhY2VAMS4xLjAsbWljcm9tYXJrLWZhY3RvcnktdGl0bGVAMS4xLjAsbWljcm9tYXJrLWZhY3Rvcnktd2hpdGVzcGFjZUAxLjEuMCxtaWNyb21hcmstdXRpbC1jaGFyYWN0ZXJAMS4yLjAsbWljcm9tYXJrLXV0aWwtY2h1bmtlZEAxLjEuMCxtaWNyb21hcmstdXRpbC1jbGFzc2lmeS1jaGFyYWN0ZXJAMS4xLjAsbWljcm9tYXJrLXV0aWwtY29tYmluZS1leHRlbnNpb25zQDEuMS4wLG1pY3JvbWFyay11dGlsLWRlY29kZS1udW1lcmljLWNoYXJhY3Rlci1yZWZlcmVuY2VAMS4xLjAsbWljcm9tYXJrLXV0aWwtZGVjb2RlLXN0cmluZ0AxLjEuMCxtaWNyb21hcmstdXRpbC1lbmNvZGVAMS4xLjAsbWljcm9tYXJrLXV0aWwtaHRtbC10YWctbmFtZUAxLjIuMCxtaWNyb21hcmstdXRpbC1ub3JtYWxpemUtaWRlbnRpZmllckAxLjEuMCxtaWNyb21hcmstdXRpbC1yZXNvbHZlLWFsbEAxLjEuMCxtaWNyb21hcmstdXRpbC1zYW5pdGl6ZS11cmlAMS4yLjAsbWljcm9tYXJrLXV0aWwtc3VidG9rZW5pemVAMS4xLjAsbWljcm9tYXJrLXV0aWwtc3ltYm9sQDEuMS4wLG1pY3JvbWFyay11dGlsLXR5cGVzQDEuMS4wLG1pY3JvbWFya0AzLjIuMCxtcmlAMS4yLjAsbXNAMi4xLjMsb2JqZWN0LWFzc2lnbkA0LjEuMSxwcm9wLXR5cGVzQDE1LjguMSxwcm9wZXJ0eS1pbmZvcm1hdGlvbkA2LjUuMCxyZWFjdC1pc0AxOC4zLjEscmVhY3RAMTguMy4xLHJlbWFyay1wYXJzZUAxMC4wLjIscmVtYXJrLXJlaHlwZUAxMC4xLjAsc2FkZUAxLjguMSxzcGFjZS1zZXBhcmF0ZWQtdG9rZW5zQDIuMC4yLHN0eWxlLXRvLW9iamVjdEAwLjQuNCx0cmltLWxpbmVzQDMuMC4xLHRyb3VnaEAyLjIuMCx1bmlmaWVkQDEwLjEuMix1bmlzdC11dGlsLWdlbmVyYXRlZEAyLjAuMSx1bmlzdC11dGlsLWlzQDUuMi4xLHVuaXN0LXV0aWwtcG9zaXRpb25ANC4wLjQsdW5pc3QtdXRpbC1zdHJpbmdpZnktcG9zaXRpb25AMy4wLjMsdW5pc3QtdXRpbC12aXNpdC1wYXJlbnRzQDUuMS4zLHVuaXN0LXV0aWwtdmlzaXRANC4xLjIsdXZ1QDAuNS42LHZmaWxlLW1lc3NhZ2VAMy4xLjQsdmZpbGVANS4zLjc/es2022/react-markdown.mjs";
export { default } from "/react-markdown@8.0.7/X-ZGJhaWxAMi4wLjIsY2hhcmFjdGVyLWVudGl0aWVzQDIuMC4yLGNvbW1hLXNlcGFyYXRlZC10b2tlbnNAMi4wLjMsZGVidWdANC40LjAsZGVjb2RlLW5hbWVkLWNoYXJhY3Rlci1yZWZlcmVuY2VAMS4wLjIsZGVxdWFsQDIuMC4zLGRpZmZANS4yLjAsZXh0ZW5kQDMuMC4yLGhhc3QtdXRpbC13aGl0ZXNwYWNlQDIuMC4xLGlubGluZS1zdHlsZS1wYXJzZXJAMC4xLjEsanMtdG9rZW5zQDQuMC4wLGtsZXVyQDQuMS41LGxvb3NlLWVudmlmeUAxLjQuMCxtZGFzdC11dGlsLWRlZmluaXRpb25zQDUuMS4yLG1kYXN0LXV0aWwtZnJvbS1tYXJrZG93bkAxLjMuMSxtZGFzdC11dGlsLXRvLWhhc3RAMTIuMy4wLG1kYXN0LXV0aWwtdG8tc3RyaW5nQDMuMi4wLG1pY3JvbWFyay1jb3JlLWNvbW1vbm1hcmtAMS4xLjAsbWljcm9tYXJrLWZhY3RvcnktZGVzdGluYXRpb25AMS4xLjAsbWljcm9tYXJrLWZhY3RvcnktbGFiZWxAMS4xLjAsbWljcm9tYXJrLWZhY3Rvcnktc3BhY2VAMS4xLjAsbWljcm9tYXJrLWZhY3RvcnktdGl0bGVAMS4xLjAsbWljcm9tYXJrLWZhY3Rvcnktd2hpdGVzcGFjZUAxLjEuMCxtaWNyb21hcmstdXRpbC1jaGFyYWN0ZXJAMS4yLjAsbWljcm9tYXJrLXV0aWwtY2h1bmtlZEAxLjEuMCxtaWNyb21hcmstdXRpbC1jbGFzc2lmeS1jaGFyYWN0ZXJAMS4xLjAsbWljcm9tYXJrLXV0aWwtY29tYmluZS1leHRlbnNpb25zQDEuMS4wLG1pY3JvbWFyay11dGlsLWRlY29kZS1udW1lcmljLWNoYXJhY3Rlci1yZWZlcmVuY2VAMS4xLjAsbWljcm9tYXJrLXV0aWwtZGVjb2RlLXN0cmluZ0AxLjEuMCxtaWNyb21hcmstdXRpbC1lbmNvZGVAMS4xLjAsbWljcm9tYXJrLXV0aWwtaHRtbC10YWctbmFtZUAxLjIuMCxtaWNyb21hcmstdXRpbC1ub3JtYWxpemUtaWRlbnRpZmllckAxLjEuMCxtaWNyb21hcmstdXRpbC1yZXNvbHZlLWFsbEAxLjEuMCxtaWNyb21hcmstdXRpbC1zYW5pdGl6ZS11cmlAMS4yLjAsbWljcm9tYXJrLXV0aWwtc3VidG9rZW5pemVAMS4xLjAsbWljcm9tYXJrLXV0aWwtc3ltYm9sQDEuMS4wLG1pY3JvbWFyay11dGlsLXR5cGVzQDEuMS4wLG1pY3JvbWFya0AzLjIuMCxtcmlAMS4yLjAsbXNAMi4xLjMsb2JqZWN0LWFzc2lnbkA0LjEuMSxwcm9wLXR5cGVzQDE1LjguMSxwcm9wZXJ0eS1pbmZvcm1hdGlvbkA2LjUuMCxyZWFjdC1pc0AxOC4zLjEscmVhY3RAMTguMy4xLHJlbWFyay1wYXJzZUAxMC4wLjIscmVtYXJrLXJlaHlwZUAxMC4xLjAsc2FkZUAxLjguMSxzcGFjZS1zZXBhcmF0ZWQtdG9rZW5zQDIuMC4yLHN0eWxlLXRvLW9iamVjdEAwLjQuNCx0cmltLWxpbmVzQDMuMC4xLHRyb3VnaEAyLjIuMCx1bmlmaWVkQDEwLjEuMix1bmlzdC11dGlsLWdlbmVyYXRlZEAyLjAuMSx1bmlzdC11dGlsLWlzQDUuMi4xLHVuaXN0LXV0aWwtcG9zaXRpb25ANC4wLjQsdW5pc3QtdXRpbC1zdHJpbmdpZnktcG9zaXRpb25AMy4wLjMsdW5pc3QtdXRpbC12aXNpdC1wYXJlbnRzQDUuMS4zLHVuaXN0LXV0aWwtdmlzaXRANC4xLjIsdXZ1QDAuNS42LHZmaWxlLW1lc3NhZ2VAMy4xLjQsdmZpbGVANS4zLjc/es2022/react-markdown.mjs";
```

どうやらふつうに直接requestすると動くみたい。
