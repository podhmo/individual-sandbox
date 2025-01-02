import { type Context, Hono } from "jsr:@hono/hono@4.6.15";
import { HTML, tsxToJs } from "jsr:@podhmo/glue@0.2.1/mini-webapp";

// serve for development
// $ deno run -A jsr:@podhmo/glue@0.2.1 serve --port 8080 ./main.ts
//
// bunle to single html file
// $ deno run -A jsr:@podhmo/glue@0.2.1 bundle --output-style html --html-id app ./client.tsx > index.html

const app = new Hono();
app.get("/", async (ctx: Context) => {
  const code = await tsxToJs("./client.tsx");
  const html = HTML({ code, id: "app", title: "Counter" });
  return ctx.html(html);
});

app.get("/readme", async (ctx: Context) => {
  const readme = await Deno.readTextFile("./README.md");
  return ctx.json({ text: readme });
});
export default app;
