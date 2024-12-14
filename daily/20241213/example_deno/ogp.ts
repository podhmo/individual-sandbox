import { DOMParser } from "jsr:@b-fuze/deno-dom@0.1.48/wasm";

import { parseArgs } from "jsr:@podhmo/with-help@0.5.2";
import { type Fetch, withTrace } from "jsr:@podhmo/build-fetch@0.1.0";

async function main() {
    const options = parseArgs(Deno.args, {
        name: "ogp",
        string: ["url"],
        required: ["url"],
        boolean: ["debug", "trace"],
    });

    const fetch = options.debug
        ? withTrace(globalThis.fetch)
        : globalThis.fetch;
    const ogp = await fetchOGP(fetch, options.url);
    console.log("%o", ogp);

    if (options.trace) {
        const res = await fetch(options.url);
        await Deno.writeTextFile("/tmp/ogp.html", await res.text());
    }
}

const OGP_FETCH_TIMEOUT = 5000; // 5 seconds

// Helper function to fetch OGP data
async function fetchOGP(fetch: Fetch, url: string) {
    const controller = new AbortController();
    const timeout = setTimeout(() => controller.abort(), OGP_FETCH_TIMEOUT);

    try {
        const response = await fetch(url, {
            signal: controller.signal,
            headers: {
                "Accept":
                    "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
                "User-Agent":
                    "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:133.0) Gecko/20100101 Firefox/133.0",
                "Cookie":
                    'session-id=358-1441746-2129522; session-id-time=2082787201l; i18n-prefs=JPY; csm-hit=tb:s-21XMRCMDNYXK8XYHW7JZ|1734146153704&t:1734146155436&adb:adblk_no; ubid-acbjp=358-2955966-3692951; session-token="vSI0Notv+215S1R6AnTY19wmtUm9HxxvGfBkQOOZRaHFqSC++jTFrloND0Edhtx1OYcu28/BP1rYPb0fDq6BNfqhIY+MNe+5RUdd8xvnVNoxaxlmdc/n7JOVM2NxGLVqqq9w72NIuf/u+VnCUQNIAPlBhW3XDWK5n+x5eIlzOYVPjvK0cGtn1ejqZbFcfCLJ3rAOpepDVuQMGjIlYGlvEx0Rah6DWBI/1lOsGU++xduhEtjLAgvWePhwL9j4uup8/xyqyiWX5duozJGJLb42j//jrlO+MHDCCUYXlIMZZ087cPMw4kIDGUOIDs2MCfQZACfy3txOUwndB91u5mAitmoBwo8PwxL6JkiT4LQQhCo="',
            },
        });
        const html = await response.text();

        const parser = new DOMParser();
        const doc = parser.parseFromString(html, "text/html");

        const r = { property: {}, name: {} };
        {
            const props: Record<string, string> = {};
            for (const el of doc.querySelectorAll("meta[property]")) {
                const name = el.getAttribute("property");
                if (name !== null) {
                    props[name] = el.getAttribute("content") ?? "";
                }
                props["title"] = doc.querySelector("title")?.textContent || "";
            }
            r.property = props;
            // console.dir(props, { depth: null });
        }

        {
            const props: Record<string, string> = {};
            for (const el of doc.querySelectorAll("meta[name]")) {
                const name = el.getAttribute("name");
                if (name !== null) {
                    props[name] = el.getAttribute("content") ?? "";
                }
            }
            r.name = props;
            // console.dir(props, { depth: null });
        }
        return r;
    } catch (error) {
        console.error("Failed to fetch OGP data", error);
        return null;
    } finally {
        clearTimeout(timeout);
    }
}

if (import.meta.main) {
    await main();
}
