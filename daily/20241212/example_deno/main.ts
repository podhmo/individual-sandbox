import "jsr:@std/dotenv/load";
import { parseArgs } from "jsr:@podhmo/with-help@0.5.2";
import { promptSecret } from "jsr:@std/cli@1.0.8/prompt-secret";
import { withTrace } from "jsr:@podhmo/build-fetch@0.1.0";

async function main() {
    // login
    const args = parseArgs(Deno.args, {
        string: ["identifier", "password"],
        boolean: ["debug"],
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

    let fetch = globalThis.fetch;
    if (args.debug) {
        fetch = withTrace(fetch);
    }
    const res = await Bluesky.login(fetch, { identifier, password });
    if (!res.ok) {
        console.error("login failed", res.status, await res.text());
        return;
    }

    console.log("%clogin success", "color: green; font-weight: bold");
    const session = await res.json();
    const lines: string[] = [];
    lines.push("");
    lines.push(`BSKY_ACCESS_TOKEN = ${session.accessJwt}`);
    lines.push(`BSKY_REFRESH_TOKEN = ${session.refreshJwt}`);

    if (args.debug) {
        console.debug("save to .env");
    }
    await Deno.writeTextFile(".env", lines.join("\n"), { append: true });
}

// deno-lint-ignore no-namespace
namespace Bluesky {
    export const baseUrl = "https://bsky.social/xrpc";

    export interface LoginInput {
        identifier: string;
        password: string;
    }

    export interface LoginOutput {
        did: string;
        didDoc: {
            "@context": string[];
            id: string;
            alsoKnownAs: string[];
            verificationMethod: {
                id: string;
                type: string;
                controller: string;
                publicKeyMultibase: string;
            }[];
            service: {
                id: string;
                type: string;
                serviceEndpoint: string;
            }[];
        };
        handle: string;
        email: string;
        emailConfirmed: boolean;
        emailAuthFactor: boolean;
        accessJwt: string; // need this
        refreshJwt: string; // and this
        active: boolean;
    }

    export function login(
        fetch: typeof globalThis.fetch,
        input: LoginInput,
    ): Promise<Response & { json: () => Promise<LoginOutput> }> {
        return fetch(`${baseUrl}/com.atproto.server.createSession`, {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
            },
            body: JSON.stringify(input),
        });
    }
}

if (import.meta.main) {
    await main();
}
