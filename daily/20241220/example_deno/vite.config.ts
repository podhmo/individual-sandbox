import { defineConfig } from "npm:vite";
import { react } from "npm:@vitejs/plugin-react";
import deno from "npm:@deno/vite-plugin";

// TODO: type declaration for defineConfig

export default defineConfig({
    plugins: [deno(), react({})],
});
