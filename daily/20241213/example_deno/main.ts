import { BskyAgent, RichText } from "@atproto/api";

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

// Helper function to refresh accessJwt
async function refreshAccessJwt(agent: BskyAgent) {
    try {
        const { data } = await agent.refreshSession({
            refreshJwt: cache.refreshJwt,
        });
        cache.accessJwt = data.accessJwt;
        cache.refreshJwt = data.refreshJwt;
        return cache.accessJwt;
    } catch (error) {
        console.error("Failed to refresh accessJwt", error);
        throw new Error("Session refresh failed");
    }
}

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
    rootPostUri?: string,
) {
    if (!cache.accessJwt) {
        throw new Error("User is not logged in.");
    }

    let replyUri = rootPostUri;

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
        const embed = ogpData && ogpData.ogImage
            ? {
                images: [
                    {
                        alt: ogpData.ogDescription || "",
                        thumb: {
                            cid: "",
                            mimeType: "image/jpeg",
                            url: ogpData.ogImage,
                        },
                        fullsize: {
                            cid: "",
                            mimeType: "image/jpeg",
                            url: ogpData.ogImage,
                        },
                    },
                ],
            }
            : undefined;

        try {
            const { uri } = await agent.post({
                text: richText.text,
                facets: richText.facets,
                reply: replyUri
                    ? { root: rootPostUri, parent: replyUri }
                    : undefined,
                embed,
            });

            replyUri = uri; // Update for next post in the thread
        } catch (error) {
            console.error("Failed to post to Bluesky", error);
            throw new Error("Post failed");
        }
    }
}

(async () => {
    const agent = new BskyAgent({ service: BLUESKY_LOGIN_URL });

    // Replace with your credentials
    const identifier = "your-username";
    const password = "your-password";

    try {
        if (!cache.did || !cache.accessJwt) {
            await login(agent, identifier, password);
        }

        // Example post
        const contents = [
            "Check out this amazing link: https://example.com",
            "Here is another follow-up message in the thread.",
        ];

        await postToBluesky(agent, contents);
    } catch (error) {
        console.error(error);
    }
})();
