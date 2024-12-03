import { parseArgs } from "jsr:@podhmo/with-help@0.5.2";
import "jsr:@std/dotenv/load"; // dotenv

// echo "GH_API_TOKEN=$(gh auth token)" > .env
// deno run -A main2.ts --filename=README.md --content="hello gist"

async function main() {
    const args = parseArgs(Deno.args, {
        description: "今度はweb APIを叩いてgistに送ってみる",
        string: ["filename", "content", "api-token"],
        boolean: ["public"],
        required: ["filename", "content", "api-token"],
        default: {
            filename: "README.md",
        },
        envvar: {
            "api-token": "GH_API_TOKEN",
        },
    });

    // https://docs.github.com/en/rest/gists/gists?apiVersion=2022-11-28#create-a-gist
    const response = await fetch("https://api.github.com/gists", {
        method: "POST",
        headers: {
            "Authorization": `Bearer ${args["api-token"]}`,
            "Accept": "application/vnd.github.v3+json",
            "X-GitHub-Enterprise-Version": "2022-11-28",
            "Content-Type": "application/json",
        },
        body: JSON.stringify({
            "public": args.public,
            "files": {
                [args.filename]: {
                    "content": args.content,
                },
            },
        }),
    });

    if (!response.ok) {
        console.error(
            `failed to create a gist: ${response.status} ${response.statusText}\n ${await response
                .text()}`,
        );
        return;
    }
    const data = await response.json();
    console.log(JSON.stringify(data, null, 2));
}

if (import.meta.main) {
    await main();
}
