import "jsr:@std/dotenv/load";
import { parseArgs, printHelp } from "jsr:@podhmo/with-help@0.5.2";
import { promptSecret } from "jsr:@std/cli@1.0.8/prompt-secret";
import { withTrace } from "jsr:@podhmo/build-fetch@0.1.0";

export async function main() {
    const baseOptions = parseArgs(Deno.args, {
        name: "bsky",
        boolean: ["debug"],
        stopEarly: true, // for subcommand
        footer: `
Available Commands:
  auth: authentication for bluesky
  post: post to bluesky`,
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
        case "post":
            await PostCommand.main(args.slice(1), baseOptions);
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
export namespace PostCommand {
    export async function main(
        baseArgs: string[],
        baseOptions: { debug: boolean },
    ) {
        const _options = parseArgs(baseArgs, {
            name: "bsky post",
            boolean: ["debug"],
            string: ["access-token", "did"],
            required: ["access-token", "did"],
            default: {
                debug: baseOptions.debug,
            },
            envvar: {
                "access-token": "BSKY_ACCESS_TOKEN",
                "did": "BSKY_DID",
            },
        });

        const options = { ...baseOptions, ..._options };
        const fetch = Bluesky.buildFetch(globalThis.fetch, {
            accessJwt: options["access-token"],
            debug: options.debug,
        });

        const res = await Bluesky.post(fetch, {
            did: options.did,
            contents: options._,
        });
        if (!res.ok) {
            console.error("post failed", res.status, await res.text());
            return;
        }
        console.log(
            "post success%o",
            await res.json(),
        );
    }
}

// deno-lint-ignore no-namespace
export namespace AuthCommand {
    export interface BaseOptions {
        debug: boolean;
        baseUrl: string;
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
    status: show login status
    token: show token
    refresh: refresh token`,
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
            case "token":
                token(args.slice(1), options);
                break;
            case "refresh":
                await refresh(args.slice(1), options);
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

    /** login */
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

        const res = await Bluesky.login(fetch, {
            identifier,
            password,
            debug: options.debug,
        });
        if (!res.ok) {
            console.error("login failed", res.status, await res.text());
            return;
        }

        console.log("%clogin success", "color: green; font-weight: bold");
        const session = await res.json();
        const lines: string[] = [];
        lines.push("");
        lines.push(`# login ${new Date().toISOString()}`);
        lines.push("BSKY_HANDLE = " + session.handle);
        lines.push("BSKY_DID = " + session.did);
        lines.push(`BSKY_ACCESS_TOKEN = ${session.accessJwt}`);
        lines.push(`BSKY_REFRESH_TOKEN = ${session.refreshJwt}`);

        if (options.debug) {
            console.debug("save to .env");
        }
        await Deno.writeTextFile(".env", lines.join("\n"), { append: true });
    }

    /** show login status */
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

        const fetch = Bluesky.buildFetch(globalThis.fetch, {
            accessJwt: options["access-token"],
            debug: options.debug,
        });
        const res = await Bluesky.getProfile(fetch, {
            actor: options.actor, // TODO: get actor from access token
        });
        if (!res.ok) {
            console.error(
                `  ❌️ Logged in to ${options.baseUrl} with invalid token[${res.status}]`,
            );
            return;
        }

        console.log(`  ✅️ Logged in to ${options.baseUrl}`);
        const profile = await res.json();
        console.log(`  handle: ${profile.handle}`);
        console.log(`  display name: ${profile.displayName}`);
        console.log(
            `  access token: ${
                options["access-token"].replaceAll(/./g, "*").substring(0, 10)
            }... length=${options["access-token"].length}`,
        );
        // console.log("%o", profile);
    }

    /** show token */
    function token(args: string[], baseOptions: BaseOptions) {
        const _options = parseArgs(args, {
            name: "bsky auth token",
            string: ["access-token"],
            boolean: ["debug"],
            required: ["access-token"],
            envvar: {
                "access-token": "BSKY_ACCESS_TOKEN",
            },
        });
        const options = { ...baseOptions, ..._options };

        console.log(`${options["access-token"]}`);
    }

    async function refresh(args: string[], baseOptions: BaseOptions) {
        const _options = parseArgs(args, {
            name: "bsky auth refresh",
            string: ["access-token", "refresh-token", "handle", "did"],
            boolean: ["debug"],
            required: ["access-token", "refresh-token", "handle", "did"],
            envvar: {
                "access-token": "BSKY_ACCESS_TOKEN",
                "refresh-token": "BSKY_REFRESH_TOKEN",
                "handle": "BSKY_HANDLE",
                "did": "BSKY_DID",
            },
        });
        const options = { ...baseOptions, ..._options };

        const res = await Bluesky.refresh(globalThis.fetch, {
            refreshJwt: options["refresh-token"],
            debug: options.debug,
        });
        if (!res.ok) {
            console.error("refresh failed", res.status, await res.text());
            return;
        }

        console.log("%crefresh success", "color: green; font-weight: bold");
        const session = await res.json();
        const lines: string[] = [];
        lines.push("");
        lines.push(`# refreshed at ${new Date().toISOString()}`);
        lines.push("BSKY_HANDLE = " + session.handle);
        lines.push("BSKY_DID = " + session.did);
        lines.push(`BSKY_ACCESS_TOKEN = ${session.accessJwt}`);
        lines.push(`BSKY_REFRESH_TOKEN = ${session.refreshJwt}`);

        if (options.debug) {
            console.debug("save to .env");
        }
        await Deno.writeTextFile(".env", lines.join("\n"), { append: true });
    }
}

// deno-lint-ignore no-namespace
export namespace Bluesky {
    export const BASE_URL = "https://bsky.social/xrpc";

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
        input: {
            identifier: string;
            password: string;
        } & { debug: boolean },
    ): Promise<Response & { json: () => Promise<LoginOutput> }> {
        if (input.debug) {
            fetch = withTrace(fetch);
        }
        return fetch(`${BASE_URL}/com.atproto.server.createSession`, {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
            },
            body: JSON.stringify(input),
        });
    }

    export interface RefreshOutput {
        refreshJwt: string;
        handle: string;
        did: string;
        didDoc?: unknown; // TODO
        active?: boolean;
        status?: string; // [active, takedown, suspended, deactivated]?
    }

    /** refresh https://docs.bsky.app/docs/api/com-atproto-server-refresh-session */
    export function refresh(
        _fetch: typeof globalThis.fetch,
        input:
            & {
                refreshJwt: string;
            }
            & { debug: boolean },
    ): Promise<Response & { json: () => Promise<RefreshOutput> }> {
        const fetch = buildFetch(_fetch, {
            accessJwt: input.refreshJwt, // requires auth using the `refreshJwt` (not the `accessJwt`)
            debug: input.debug,
        });
        return fetch(`${BASE_URL}/com.atproto.server.refreshSession`, {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
            },
        });
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
        input: {
            /** actor is Handle or DID of account*/
            actor: string;
        },
    ): Promise<Response & { json: () => Promise<GetProfileOutput> }> {
        return await fetch(
            `${BASE_URL}/app.bsky.actor.getProfile?actor=${input.actor}`,
            {
                method: "GET",
            },
        );
    }

    export interface PostOutput {
        uri: string;
        cid: string;
        commit: {
            cid: string;
            rev: string;
        };
        validationStatus: string; // "valid" | "invalid"?
    }

    /** post  https://docs.bsky.app/docs/api/com-atproto-repo-create-record */
    export async function post(
        fetch: Fetch,
        input: {
            did: string; // DID of the account

            contents: string[];
            createdAt?: string;
        },
    ): Promise<Response & { json: () => Promise<PostOutput> }> {
        // TODO: backoff
        // TODO: mentions and links https://docs.bsky.app/docs/advanced-guides/posts#mentions-and-links

        const repo = input.did;

        // for replies https://docs.bsky.app/docs/advanced-guides/posts#replies
        let root: { uri: string; cid: string } | undefined = undefined;
        let parent: { uri: string; cid: string } | undefined = undefined;

        let i = 0;
        for (const content of input.contents) {
            const createdAt = input.createdAt ?? new Date().toISOString(); // 現在時刻をISO 8601形式で取得

            const res = await fetch(
                `${BASE_URL}/com.atproto.repo.createRecord`,
                {
                    method: "POST",
                    headers: {
                        "Content-Type": "application/json",
                    },
                    body: JSON.stringify({
                        repo,
                        collection: "app.bsky.feed.post",
                        record: { // https://github.com/bluesky-social/atproto/blob/main/lexicons/app/bsky/feed/post.json#L9
                            $type: "app.bsky.feed.post",
                            text: content,
                            createdAt,
                            // langs: ["ja", "en"], // TODO: langs
                            reply: root ? { root, parent } : undefined, // https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/repo/strongRef.json
                        },
                    }),
                },
            );
            if (!res.ok) {
                return res;
            }

            i++;
            if (i === input.contents.length) {
                return res;
            }

            const { uri, cid } = await res.json();
            parent = { uri, cid };
            if (root === undefined) {
                root = { ...parent };
            }
        }
        throw new Error("unreachable"); // never
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
