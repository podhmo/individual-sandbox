import { parseArgs } from "jsr:@podhmo/with-help@0.5.2";
import { promptSecret } from "jsr:@std/cli@1.0.8/prompt-secret";
import "jsr:@std/dotenv/load";

// login
const args = parseArgs(Deno.args, {
    string: ["identifier", "password"],
    required: [],
    envvar: {
        identifier: "BSKY_IDENTIFIER",
        password: "BSKY_PASSWORD",
    },
});

let { identifier, password } = args;
console.log("%cneed login", "color: blue; font-weight: bold");
if (identifier === undefined) {
    let input = null;
    while (input === "" || input === null) {
        input = promptSecret("  identifier>");
    }
    identifier = input.trim();
}

if (password === undefined) {
    let input = null;
    while (input === "" || input === null) {
        input = promptSecret("  password>");
    }
    password = input.trim();
}

console.log({ identifier, password });
