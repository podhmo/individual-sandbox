import "jsr:@std/dotenv/load";
import { parseArgs, printHelp } from "jsr:@podhmo/with-help@0.5.2";
import { promptSecret } from "jsr:@std/cli@1.0.8/prompt-secret";
import { withTrace } from "jsr:@podhmo/build-fetch@0.1.0";

interface BaseOptions {
    debug: boolean;
}

async function main() {
    const baseOptions = parseArgs(Deno.args, {
        name: "bsky",
        boolean: ["debug"],
        stopEarly: true, // for subcommand
        footer: `
Available Commands:
  auth: authentication for bluesky`,
    });

    const args = baseOptions._;
    if (args.length === 0) {
        console.error("%cneed command", "color: red; font-weight: bold");
        printHelp(baseOptions);
        return;
    }
    switch (args[0]) {
        case "auth":
            await AuthCommand.main(args.slice(1), baseOptions);
            break;
        default:
            console.error(
                `%cunknown command: ${args[0]}`,
                "color: red; font-weight: bold",
            );
            printHelp(baseOptions);
            break;
    }
}

// deno-lint-ignore no-namespace
namespace AuthCommand {
    export interface BaseOptions {
        debug: boolean;
        baseUrl: string;
    }

    export async function login(args: string[], baseOptions: BaseOptions) {
        const options = parseArgs(args, {
            name: "bsky auth login",
            string: ["identifier", "password"],
            boolean: ["debug"],
            required: [],
            default: {
                debug: baseOptions.debug,
            },
            envvar: {
                identifier: "BSKY_IDENTIFIER",
                password: "BSKY_PASSWORD",
            },
        });

        let { identifier, password } = options;
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
        if (options.debug) {
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
        lines.push("BSKY_HANDLE = " + session.handle);
        lines.push(`BSKY_ACCESS_TOKEN = ${session.accessJwt}`);
        lines.push(`BSKY_REFRESH_TOKEN = ${session.refreshJwt}`);

        if (options.debug) {
            console.debug("save to .env");
        }
        await Deno.writeTextFile(".env", lines.join("\n"), { append: true });
    }

    export async function status(args: string[], baseOptions: BaseOptions) {
        const _options = parseArgs(args, {
            name: "bsky auth status",
            string: ["access-token", "actor"],
            boolean: ["debug"],
            required: ["access-token", "actor"],
            envvar: {
                "access-token": "BSKY_ACCESS_TOKEN",
                "actor": "BSKY_HANDLE", // TODO: get actor from access token
            },
        });

        const options = { ...baseOptions, ..._options };

        console.log(`%c${options.baseUrl}`, "font-weight: bold");
        if (options["access-token"] === undefined) {
            console.error(`  ❌️ Logged in to ${options.baseUrl}`);
            return;
        }
        console.log(`  ✅️ Logged in to ${options.baseUrl}`);

        const fetch = Bluesky.buildFetch(globalThis.fetch, {
            accessJwt: options["access-token"],
            debug: options.debug,
        });

        const profile = await Bluesky.getProfile(fetch, {
            actor: options.actor, // TODO: get actor from access token
        });
        console.log(`  handle: ${profile.handle}`);
        console.log(`  display name: ${profile.displayName}`);
        console.log(
            `  access token: ${
                options["access-token"].replaceAll(/./g, "*").substring(0, 10)
            }... length=${options["access-token"].length}`,
        );
        // console.log("%o", profile);
    }

    export async function main(
        baseArgs: string[],
        baseOptions: { debug: boolean },
    ) {
        const options = parseArgs(baseArgs, {
            name: "bsky auth",
            boolean: ["debug"],
            string: ["baseUrl"],
            required: ["baseUrl"],
            default: {
                debug: baseOptions.debug,
                baseUrl: Bluesky.BASE_URL,
            },
            stopEarly: true, // for subcommand
            footer: `
    Available Commands:
      login:  login to bluesky
      status: show login status`,
        });

        const args = options._;
        if (args.length === 0) {
            console.error("%cneed command:", "color: red; font-weight: bold");
            printHelp(options);
            return;
        }
        switch (args[0]) {
            case "login":
                await login(args.slice(1), options);
                break;
            case "status":
                await status(args.slice(1), options);
                break;
            default:
                console.error(
                    `%cunknown command: ${args[0]}`,
                    "color: red; font-weight: bold",
                );
                printHelp(options);
                break;
        }
    }
}

// deno-lint-ignore no-namespace
namespace Bluesky {
    export const BASE_URL = "https://bsky.social/xrpc";

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

    /** login */
    export function login(
        fetch: typeof globalThis.fetch,
        input: LoginInput,
    ): Promise<Response & { json: () => Promise<LoginOutput> }> {
        return fetch(`${BASE_URL}/com.atproto.server.createSession`, {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
            },
            body: JSON.stringify(input),
        });
    }

    export interface GetProfileInput {
        /** actor is Handle or DID of account*/
        actor: string;
    }
    export interface GetProfileOutput {
        did: string;
        handle: string;
        displayName: string;
        avatar: string;
        associated: {
            lists: number;
            feedgens: number;
            starterPacks: number;
            labeler: boolean;
            chat: {
                allowIncoming: string;
            };
        };
        viewer: {
            muted: boolean;
            blockedBy: boolean;
            knownFollowers: {
                count: number;
                followers: {
                    did: string;
                    handle: string;
                    displayName: string;
                    avatar: string;
                    associated: unknown; // TODO
                    viewer: unknown; // TODO
                    labels: unknown[]; // TODO
                    createdAt: string;
                }[];
            };
        };
        labels: unknown[]; // TODO
        createdAt: string;
        description: string;
        indexedAt: string;
        followersCount: number;
        followsCount: number;
        postsCount: number;
    }

    /** get profile */
    export async function getProfile(
        fetch: Fetch,
        input: GetProfileInput,
    ): Promise<GetProfileOutput> {
        const res = await fetch(
            `${BASE_URL}/app.bsky.actor.getProfile?actor=${input.actor}`,
            {
                method: "GET",
            },
        );
        return await res.json();
    }

    export type Fetch = (
        url: string,
        init?: RequestInit & { headers?: Record<string, string> },
    ) => Promise<Response>;

    export function buildFetch(
        inner: typeof globalThis.fetch,
        options: { accessJwt: string; baseUrl?: string } & { debug?: boolean },
    ) {
        if (options.debug) {
            inner = withTrace(inner);
        }
        let baseUrl = options.baseUrl ?? BASE_URL;
        if (!baseUrl.endsWith("/")) {
            baseUrl = baseUrl.substring(0, baseUrl.length - 1);
        }

        return async function fetch(
            url: string, // url or path
            init?: RequestInit & { headers?: Record<string, string> }, // Iterable<string[]> を省略
        ): Promise<Response> {
            init = init ?? {};
            const headers = init.headers ?? {};

            if (url.startsWith("/")) {
                url = `${baseUrl}${url}`;
            }
            if (url.startsWith(baseUrl)) {
                headers["Authorization"] = `Bearer ${options.accessJwt}`;
            }
            return await inner(url, { ...init, headers });
        };
    }
}

if (import.meta.main) {
    await main();
}
