// deno serve --allow-net 01hello.ts --host 127.0.0.1 --port 8080

export default {
    fetch: (_: Request) => {
        return new Response(`{"message": "hello deno"}`, {
            status: 200,
            headers: {
                "content-type": "application/json",
            },
        });
    },
};
