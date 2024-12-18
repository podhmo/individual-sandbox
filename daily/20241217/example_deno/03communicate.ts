export function indexPage(_req: Request): Response {
  const html = `
<!DOCTYPE html>
<html lang="ja">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Hello</title>
</head>
<body>
  <form>
    <h1>Hello, someone</h1>
    <input type="text" id="name" placeholder="Enter your name">
    <button id="submit">Submit</button>
  </form>
  <script type="module">
    const form = document.querySelector("form");
    const name = document.querySelector("#name");
    form.addEventListener("submit", async (e) => {
      e.preventDefault();
      const res = await fetch("/api/hello/" + name.value, {headers: {"content-type": "application/json"}});
      const data = await res.json(); // {"message": "Hello, <someone>!"}
      document.querySelector("h1").textContent = data.message;
    });
  </script>
</body>
        `;
  return new Response(html, {
    status: 200,
    headers: { "content-type": "text/html" },
  });
}

export function helloAPI(_req: Request, name: string | undefined): Response {
  return new Response(`{"message": "Hello, ${name}!"}`, {
    status: 200,
    headers: { "content-type": "application/json" },
  });
}

// $ deno serve --port 8080 --allow-net 03communicate.ts
export default {
  fetch: (req: Request): Response => {
    const contenType = req.headers.get("content-type");
    if (contenType !== "application/json") {
      return indexPage(req);
    }

    const route = new URLPattern({ pathname: "/api/hello/:name" });
    const match = route.exec(req.url);
    if (match) {
      const name = match.pathname.groups.name;
      return helloAPI(req, name);
    }

    return new Response(`{"error": "Not found"}`, {
      status: 404,
      headers: { "content-type": "application/json" },
    });
  },
};
