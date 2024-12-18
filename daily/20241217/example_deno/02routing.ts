// $ deno serve --port 8080 --allow-net 02routing.ts

const route = new URLPattern({ pathname: "/hello/:name" });
export default {
    fetch: (req: Request): Response => {
        const match = route.exec(req.url);
        if (match) {
            const name = match.pathname.groups.name;
            return new Response(`{"message": "Hello, ${name}!"}`, {
                status: 200,
                headers: { "content-type": "application/json" },
            });
        }

        return new Response(`{"error": "Not found"}`, {
            status: 404,
            headers: { "content-type": "application/json" },
        });
    },
};
