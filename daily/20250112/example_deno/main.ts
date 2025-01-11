import { join, join as pathjoin } from "jsr:@std/path@1/join";

import { type Context, Hono } from "jsr:@hono/hono@4.6.16";
import { showRoutes } from "jsr:@hono/hono@4.6.16/dev";
import { moreStrict, parseArgs } from "jsr:@podhmo/with-help@0.5.3";

import { CODE, HTML, LINKS, tsxToJs } from "jsr:@podhmo/glue@0.2.4/mini-webapp";
import { serve } from "jsr:@podhmo/glue@0.2.4/serve";
import {
  BASE_URL as ESM_SH_BASE_URL,
  NEXT_BASE_URL as ESM_SH_NEXT_BASE_URL,
} from "jsr:@podhmo/glue@0.2.4/esm-sh";

const app = new Hono();

const tsxFiles: string[] = [];
for (const e of Deno.readDirSync(".")) {
  if (e.isFile && e.name.endsWith(".tsx")) {
    tsxFiles.push(e.name);
  }
}

app.get("/", (ctx: Context) => {
  const title = "list of entries";
  const html = HTML({ title }, LINKS({ links: tsxFiles }));
  return ctx.html(html);
});

app.on("GET", tsxFiles, async (ctx: Context) => {
  const filename = ctx.req.path.slice(1);
  const filepath = pathjoin(import.meta.dirname ?? "", filename);

  const code = await tsxToJs(filepath);
  const html = HTML({ title: filename }, CODE({ id: "root", code }));
  return ctx.html(html);
});

function main() {
  const options_ = parseArgs(Deno.args, {
    string: ["port", "host", "deno-config"],
    required: ["port", "host"],
    boolean: ["cache", "development", "next", "debug"],
    negatable: ["cache"],
    default: {
      port: "8080",
      host: "127.0.0.1",
      "deno-config": join(import.meta.dirname ?? "", "deno.json"),
    },
    flagDescription: {
      "deno-config": "deno.json or deno.jsonc",
      "development": "development mode for esm.sh",
      "next": "set https://next.esm.sh as base url",
    },
  });
  const restrict = moreStrict(options_);
  const options = {
    ...options_,
    port: restrict.integer(options_.port),
  };

  console.log("%croutes:", "font-weight: bold;");
  console.log("");
  showRoutes(app, { verbose: false });
  console.log("");

  serve(app, {
    port: options.port,
    hostname: options.host,

    cache: options.cache,
    debug: options.debug,
    development: options.development,
    baseUrl: options.next ? ESM_SH_NEXT_BASE_URL : ESM_SH_BASE_URL,
    denoConfigPath: options["deno-config"],
  }).finished.then(() => {
    console.log(`stop http://${options.host}:${options.port}/`);
  });
}

if (import.meta.main) {
  main();
}
