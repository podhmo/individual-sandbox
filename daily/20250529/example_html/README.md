# プロンプト階層管理アプリ (Preact/Deno/Vite版)

階層構造でプロンプトテンプレートを管理し、LLMへの入力を効率化するシンプルなWebアプリケーションです。
このバージョンは Preact, htm を使用し、Deno と Vite で開発・ビルドを行います。

## 機能概要

-   `public/templates.md` に定義されたプロンプトテンプレートを「カテゴリ → テンプレートタイトル」の階層で表示
-   テンプレートを選択し、必要な変数に文字列を入力
-   最終的なプロンプトを生成し、クリップボードにコピー

## 技術スタック

-   HTML
-   CSS (Pico.css + カスタムスタイル)
-   JavaScript (ES Modules, Preact, htm)
-   Deno (ランタイム)
-   Vite (ビルドツール・開発サーバー、`npm:` 経由で使用)

## セットアップと開発

### 1. 前提条件

-   [Deno](https://deno.land/) (バージョン 1.28 以降推奨)

### 2. プロジェクトの準備

プロジェクトのルートディレクトリに `deno.json` (または `deno.jsonc`) ファイルを以下の内容で作成または編集します。
このファイルで、ViteやPreactなどの依存関係を import map として定義し、開発用タスクを設定します。

```jsonc
// deno.json
{
  "tasks": {
    "dev": "deno run --allow-read --allow-net --allow-env npm:vite",
    "build": "deno run --allow-read --allow-write --allow-net --allow-env npm:vite build",
    "preview": "deno run --allow-read --allow-net --allow-env npm:vite preview"
  },
  "imports": {
    "preact": "npm:preact@10.22.1",
    "preact/hooks": "npm:preact@10.22.1/hooks",
    "htm/preact": "npm:htm@3.1.1/preact",
    // Vite関連の型定義が必要な場合はここに追加することがあります
    // (通常はViteプラグインが型を提供するか、不要な場合が多い)
    "vite": "npm:vite@5.3.1", // vite 本体を import map に含めることも可能
    "@preact/preset-vite": "npm:@preact/preset-vite@2.8.2" // vite.config.js で使う場合
  },
  "compilerOptions": {
    "jsx": "react-jsx", // htm/preact を使う場合、jsxImportSource は不要なことが多い
    "jsxImportSource": "preact" // 必要に応じて
  },
  "lint": {
    "files": {
      "include": ["js/", "public/", "*.js", "*.json", "*.md"],
      "exclude": ["dist/"]
    },
    "rules": {
      "tags": ["recommended"],
      "include": [],
      "exclude": []
    }
  },
  "fmt": {
    "files": {
      "include": ["js/", "public/", "*.js", "*.json", "*.md"],
      "exclude": ["dist/"]
    },
    "options": {
      "useTabs": false,
      "lineWidth": 80,
      "indentWidth": 2,
      "singleQuote": true // お好みで
    }
  }
}
```
**注意:** 上記の `deno.json` のバージョン番号 (例: `preact@10.22.1`, `vite@5.3.1`) は、本回答作成時点のものです。適宜最新のバージョンに更新してください。

`vite.config.js` は前回生成されたものと同様のものがプロジェクトルートに必要です。
`package.json` は、`{ "type": "module" }` という内容でプロジェクトルートに配置しておくと、ViteがESMの挙動を正しく解釈するのに役立つ場合があります（Deno環境では必須ではないことが多いですが、念のため）。

### 3. 開発サーバーの起動

以下のコマンドを実行すると、Vite開発サーバーが起動し、ブラウザでアプリケーションが開きます。
ソースコード (`js/` ディレクトリ内のファイルや `public/templates.md`) を変更すると、ホットリロードにより即座にブラウザに反映されます。

```bash
deno task dev
```

デフォルトでは `http://localhost:5173` (またはViteが利用可能なポート) でアクセスできます。

### 4. テンプレートデータの編集

アプリケーションで利用するプロンプトテンプレートは、`public/templates.md` ファイルにMarkdown形式で定義されています。
新しいテンプレートの追加、既存テンプレートの編集・削除は、このファイルを直接編集してください。

**Markdown形式のルール:**

-   カテゴリ名は `#` で始まる行で定義します (例: `# ビジネスメール`)。
-   テンプレート名は `##` で始まる行で定義します (例: `## 社外向け挨拶メール`)。
-   テンプレート名の下に記述されたテキストは、最初のコードブロックが現れるまで、そのテンプレートの説明文として扱われます。
-   テンプレート本体は、最初に現れる3つ以上のバッククォート（\`\`\`）またはチルダ（~~~）で囲まれたコードブロック内に記述します。
    -   例:
        ```markdown
        ## テンプレート名
        これが説明文です。
        複数行にわたっても構いません。
        ```
        {{変数1}} を使ったテンプレートです。
        {{変数2}} もあります。
        ```
        ```

編集後、開発サーバーが起動していれば自動的に変更が反映されます。

### 5. 本番用ビルド

アプリケーションをデプロイ用にビルドするには、以下のコマンドを実行します。
ビルドされたファイルは `dist/` ディレクトリに出力されます。

```bash
deno task build
```

### 6. ビルドファイルのプレビュー

ビルドされたアプリケーションをローカルで確認するには、以下のコマンドを実行します。

```bash
deno task preview
```