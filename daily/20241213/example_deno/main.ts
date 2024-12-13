import { BskyAgent, RichText } from "npm:@atproto/api@0.13.20";
import { DOMParser } from "jsr:@b-fuze/deno-dom@0.1.48/wasm";
import { parseArgs } from "jsr:@podhmo/with-help@0.5.2";
import "jsr:@std/dotenv/load";
import { isAspectRatio } from "npm:@atproto/api@0.13.20";
import _brrp__multiformats_scope_baseX from "../../../../../../../.cache/deno/npm/registry.npmjs.org/multiformats/9.9.0/cjs/vendor/base-x.js";

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
    const agent = new BskyAgent({ service: BLUESKY_LOGIN_URL });
    const options = parseArgs(Deno.args, {
        string: ["identifier", "password"],
        required: ["identifier", "password"],
        envvar: {
            identifier: "BSKY_IDENTIFIER",
            password: "BSKY_PASSWORD",
        },
    });

    const { identifier, password } = options;
    try {
        if (!cache.did || !cache.accessJwt) {
            await login(agent, identifier, password);
        }

        // Example post
        const contents = [
            "これはChatGPTに生成してもらったものを整形したもの ( https://gist.github.com/podhmo/c9bcef83c88e40b38fb3eb7519b6cc56 )",
            "そのあとここでコメントを追記してほしい",
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

    interface Ref {
        uri: string;
        cid: string;
        [k: string]: unknown;
    }
    let root: Ref | undefined = undefined;
    let parent: Ref | undefined = undefined;

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
        let embed = undefined;
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
                        "$type": "blob",
                        "ref": {
                            "$link": blobRes.data.blob.ref.toString(),
                        },
                        "mimetype": blobRes.data.blob.mimeType,
                        size: blobRes.data.blob.size,
                    },
                },
            };
            embed = {
                $type: "app.bsky.embed.images",
                images: [{
                    alt: ogpData.ogTitle || "",
                    aspectRatio: { width: 191, height: 100 }, // guessing aspect ratio?
                    image: {
                        $type: "blob",
                        ref: {
                            $link: blobRes.data.blob.ref.toString(),
                        },
                        mimeType: blobRes.data.blob.mimeType,
                        size: blobRes.data.blob.size,
                    },
                }],
            };
        }

        try {
            console.dir({
                text: richText.text,
                facets: richText.facets,
                reply: root !== undefined ? { root, parent } : undefined,
                embed: embed,
            });
            const { uri, cid } = await agent.post({
                text: richText.text,
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
