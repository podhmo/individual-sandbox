import { moreStrict, parseArgs } from "jsr:@podhmo/with-help@0.5.3";

const options_ = parseArgs(Deno.args, {
    string: ["port"],
    required: ["port"],
});

const toInt = moreStrict(options_).integer;
const options = { ...options_, port: toInt(options_.port) };

// deno run --allow-net 00hello.ts --port 8080
// http :8080

const ac = new AbortController();

// setup server
Deno.serve({
    port: options.port,
    hostname: "127.0.0.1",
    signal: ac.signal,
    handler: (_: Request) => {
        return new Response(`{"message": "Hello, Deno!"}`, {
            status: 200,
            headers: {
                "content-type": "application/json; charset=utf-8",
            },
        });
    },
    onListen: ({ port, hostname }) => {
        console.log(`start listening on ${hostname}:${port}`);
    },
});

// signal handling
Deno.addSignalListener("SIGINT", () => {
    console.log("SIGINT");
    ac.abort();
});
