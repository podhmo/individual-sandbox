import * as jsonc from "jsr:@std/jsonc";
import { parseArgs } from "jsr:@podhmo/with-help@0.4.0";

// deno run --allow-read parse.ts --config config.jsonc

const args = parseArgs(Deno.args, {
    string: ["config", "apikey"],
    required: ["config", "apikey"],
    envvar: {
        apikey: "YOUTUBE_API_KEY",
    },
});

const config = jsonc.parse(Deno.readTextFileSync(args.config));
const { channels }: { channels: string[] } = JSON.parse(JSON.stringify(config));

for (let channelId of channels) {
    const baseURL = "https://www.googleapis.com/youtube/v3";

    if (channelId.startsWith("@")) {
        // extracts real channel id from alias
        const apiURL = `${baseURL}/search?part=snippet&q=${
            channelId.slice(1)
        }&type=channel&key=${args.apikey}`;
        const response = await fetch(apiURL);
        if (response.status !== 200) {
            console.log(response.status);
            throw new Error(
                `failed to fetch url=${apiURL}, text=${await response.text()}`,
            );
        }
        const data = await response.json();
        if (data.items.length === 0) {
            console.error(`channel not found: ${channelId}`);
            continue;
        }
        console.log(`channelId: ${channelId} -> ${data.items[0].id.channelId}`);
        channelId = data.items[0].id.channelId;
    }

    console.log(`channelId: ${channelId}`);
    const apiURL =
        `${baseURL}/playlists?part=id,status,snippet&channelId=${channelId}&maxResults=50&apiKey=${args.apikey}`;
    const response = await fetch(`${apiURL}&key=${args.apikey}`);
    if (response.status !== 200) {
        console.log(response.status);
        throw new Error(
            `failed to fetch url=${apiURL}, text=${await response.text()}`,
        );
    }
    const data = await response.json();
    for (const item of data.items) {
        console.log(`\t${item.snippet.title} (${item.id})`);
    }
    // console.log(JSON.stringify(data, null, 2));
}
