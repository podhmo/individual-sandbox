# viteでshadcn/uiを使ってみる

https://ui.shadcn.com/docs/installation/vite

```console
$ pnpm create vite@latest -t react-ts
$ cd vite-project

$ pnpm add -D tailwindcss postcss autoprefixer
$ pnpm dlx tailwindcss init -p

$ pnpm add -D @types/node

$ pnpm dlx shadcn@latest init
$ pnpm dlx shadcn@latest add button
```