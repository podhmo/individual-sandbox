// vite.config.js
import { defineConfig } from 'vite';
import preact from '@preact/preset-vite'; // Preactの公式プリセット

export default defineConfig({
  plugins: [preact()], // Preactプラグインを追加
  // プロジェクトのルートディレクトリ。デフォルトは process.cwd()
  // root: '.', 

  // ビルド成果物の出力先ディレクトリ。
  build: {
    outDir: 'dist',
  },

  // 開発サーバーの設定
  server: {
    port: 5173, // デフォルトは5173、変更可能
    open: true, // サーバー起動時にブラウザを自動で開く
  },

  // publicディレクトリの設定 (デフォルトは 'public')
  // publicDir: 'public',
});