// vite.config.ts
import { defineConfig } from 'vite';

export default defineConfig({
  // プロジェクトのルートディレクトリ。デフォルトは process.cwd() ですが、
  // index.html の場所によっては明示的に指定する必要があるかもしれません。
  // 通常はデフォルトで問題ありません。
  // root: '.',

  // ビルド成果物の出力先ディレクトリ。デフォルトは 'dist' です。
  build: {
    outDir: 'dist', // 出力先を 'dist' にする (デフォルト)
    // rollupOptions: { // 必要に応じてRollupのオプションをカスタマイズ
    //   // ...
    // }
  },

  // 開発サーバーの設定
  server: {
    port: 3000, // 開発サーバーのポート番号 (任意)
    open: true, // サーバー起動時にブラウザを自動で開く
  },

  // TypeScriptのエイリアス設定などが必要な場合は `resolve.alias` を使います。
  // 今回の構成では特に不要ですが、参考までに。
  // resolve: {
  //   alias: {
  //     '@': path.resolve(__dirname, './ts') // 例: '@/data' のようにインポート可能に
  //   }
  // },

  // CSSのプリプロセッサ（Sass, Lessなど）を使用する場合は、
  // ここに設定を追加します。今回はPico.cssのCDN版と素のCSSなので不要です。

  // 静的アセットの扱いに関する設定。
  // publicDir: 'public', // デフォルトは 'public' ディレクトリ。
                       // ここに置いたファイルはビルド時に dist ルートにコピーされます。
                       // favicon.icoなどを置くのに便利。

  // ES Moduleの依存関係の最適化に関する設定。
  // optimizeDeps: {
  //   // ...
  // }
});