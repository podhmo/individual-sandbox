import { BskyAgent, RichText } from "npm:@atproto/api@0.13.20";
import { type Main as ComAtprotoRepoStrongRef } from "npm:@atproto/api@0.13.20/dist/client/types/com/atproto/repo/strongRef.d.ts";
import { type Main as AppBskyEmbedExternal } from "npm:@atproto/api@0.13.20/dist/client/types/app/bsky/embed/external.d.ts";
import { DOMParser } from "jsr:@b-fuze/deno-dom@0.1.48/wasm";
import { parseArgs } from "jsr:@podhmo/with-help@0.5.2";
import { withTrace } from "jsr:@podhmo/build-fetch@0.1.0";
import "jsr:@std/dotenv/load";

// Cache for DID, accessJwt, and refreshJwt
const cache = {
    did: "",
    accessJwt: "",
    refreshJwt: "",
};

// Constants
const BLUESKY_LOGIN_URL =
    "https://bsky.social/xrpc/com.atproto.server.createSession";
const OGP_FETCH_TIMEOUT = 5000; // 5 seconds

async function main() {
    const options = parseArgs(Deno.args, {
        string: ["identifier", "password"],
        required: ["identifier", "password"],
        boolean: ["debug"],
        envvar: {
            identifier: "BSKY_IDENTIFIER",
            password: "BSKY_PASSWORD",
            debug: "DEBUG",
        },
    });

    let fetch = globalThis.fetch;
    if (options.debug) {
        fetch = withTrace(fetch);
    }
    const agent = new BskyAgent({ service: BLUESKY_LOGIN_URL, fetch: fetch });

    const { identifier, password } = options;
    try {
        if (!cache.did || !cache.accessJwt) {
            await login(agent, identifier, password);
        }

        // Example post
        const contents = [
            // "test: これはChatGPTに生成してもらったものを整形したもの ( https://gist.github.com/podhmo/c9bcef83c88e40b38fb3eb7519b6cc56 )",
            // "test: そのあとここでコメントを追記してほしい",
            // "test: リプライはどちらの形式でも動く？ `@podhmo` @podhmo ? `@podhmo.bsky.social` @podhmo.bsky.social ?",
            "test: これはスクリプトから https://www.amazon.co.jp/Kindle-6%E3%82%A4%E3%83%B3%E3%83%81%E3%83%87%E3%82%A3%E3%82%B9%E3%83%97%E3%83%AC%E3%82%A4-%E9%9B%BB%E5%AD%90%E6%9B%B8%E7%B1%8D%E3%83%AA%E3%83%BC%E3%83%80%E3%83%BC-16GB-%E3%83%96%E3%83%A9%E3%83%83%E3%82%AF-%E5%BA%83%E5%91%8A%E3%81%AA%E3%81%97/dp/B0CP31L73X/",
        ];

        await postToBluesky(agent, contents);
    } catch (error) {
        console.error(error);
    }
}

// Helper function to fetch OGP data
async function fetchOGP(url: string) {
    const controller = new AbortController();
    const timeout = setTimeout(() => controller.abort(), OGP_FETCH_TIMEOUT);

    try {
        const response = await fetch(url, { signal: controller.signal });
        const html = await response.text();

        const parser = new DOMParser();
        const doc = parser.parseFromString(html, "text/html");

        const ogTitle =
            doc.querySelector("meta[property='og:title']")?.getAttribute(
                "content",
            ) || "";
        const ogDescription =
            doc.querySelector("meta[property='og:description']")?.getAttribute(
                "content",
            ) || "";
        const ogImage =
            doc.querySelector("meta[property='og:image']")?.getAttribute(
                "content",
            ) || "";
        return { ogTitle, ogDescription, ogImage };
    } catch (error) {
        console.error("Failed to fetch OGP data", error);
        return null;
    } finally {
        clearTimeout(timeout);
    }
}

// // Helper function to refresh accessJwt
// async function refreshAccessJwt(agent: BskyAgent) {
//     try {
//         const { data } = await agent.refreshSession({
//             refreshJwt: cache.refreshJwt,
//         });
//         cache.accessJwt = data.accessJwt;
//         cache.refreshJwt = data.refreshJwt;
//         return cache.accessJwt;
//     } catch (error) {
//         console.error("Failed to refresh accessJwt", error);
//         throw new Error("Session refresh failed");
//     }
// }

// Login function
async function login(agent: BskyAgent, identifier: string, password: string) {
    const { data } = await agent.login({ identifier, password });
    cache.did = data.did;
    cache.accessJwt = data.accessJwt;
    cache.refreshJwt = data.refreshJwt;
}

// Post function with RichText and OGP support
async function postToBluesky(
    agent: BskyAgent,
    contents: string[],
) {
    if (!cache.accessJwt) {
        throw new Error("User is not logged in.");
    }

    let root: ComAtprotoRepoStrongRef | undefined = undefined;
    let parent: ComAtprotoRepoStrongRef | undefined = undefined;

    for (const content of contents) {
        // Detect links and fetch OGP data
        const urlMatch = content.match(/https?:\/\/\S+/);
        let ogpData = null;
        if (urlMatch) {
            ogpData = await fetchOGP(urlMatch[0]);
        }

        // Create RichText with facets
        const richText = new RichText({ text: content });
        await richText.detectFacets(agent);

        // Attach OGP data if available
        let embed: AppBskyEmbedExternal | undefined = undefined;
        if (ogpData && ogpData.ogImage) {
            const imageRes = await fetch(ogpData.ogImage);
            const contentType = imageRes.headers.get("content-type");
            const blob = await imageRes.blob();

            // upload blob
            let headers = {};
            if (contentType) {
                headers = { "Content-Type": contentType };
            }
            const blobRes = await agent.uploadBlob(blob, { headers });

            // https://docs.bsky.app/docs/advanced-guides/posts#images-embeds
            embed = {
                $type: "app.bsky.embed.external",
                external: {
                    uri: ogpData.ogImage,
                    title: ogpData.ogTitle || "",
                    description: ogpData.ogDescription || "",
                    thumb: {
                        $type: "blob",
                        mimeType: blobRes.data.blob.mimeType,
                        size: blobRes.data.blob.size,
                        ref: {
                            $link: blobRes.data.blob.ref.toString(),
                        },
                    },
                },
            };
        }

        try {
            const { uri, cid } = await agent.post({
                $type: "app.bsky.feed.post",
                text: richText.text,
                langs: ["ja"],
                createdAt: new Date().toISOString(),
                facets: richText.facets,
                reply: root !== undefined
                    ? { root, parent: parent ?? root }
                    : undefined,
                embed: embed,
            });

            parent = { uri, cid };
            if (root === undefined) {
                root = { uri, cid };
            }
        } catch (error) {
            console.error("Failed to post to Bluesky", error);
            throw new Error("Post failed");
        }
    }
}

if (import.meta.main) {
    await main();
}
