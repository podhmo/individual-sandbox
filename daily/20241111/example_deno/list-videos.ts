import * as jsonc from "jsr:@std/jsonc";
import { parseArgs } from "jsr:@podhmo/with-help@0.4.0";

// deno run --allow-read parse.ts --config config.jsonc

const args = parseArgs(Deno.args, {
    string: ["config", "apikey", "maxResults", "order"],
    required: ["config", "apikey", "maxResults", "order"],
    envvar: {
        apikey: "YOUTUBE_API_KEY",
    },
    default: {
        maxResults: "50",
        order: "date",
    },
});

const config = jsonc.parse(Deno.readTextFileSync(args.config));
const { channels }: { channels: string[] } = JSON.parse(JSON.stringify(config));

const publishedAfter = "2024-04-01T00:00:00Z";

for (let channelId of channels) {
    const baseURL = "https://www.googleapis.com/youtube/v3";
    const channel = {
        id: channelId,
        title: channelId,
    };

    if (channelId.startsWith("@")) {
        // extracts real channel id from alias
        const apiURL = `${baseURL}/search?part=snippet&q=${
            channelId.slice(1)
        }&type=channel&key=${args.apikey}`;
        const response = await fetch(apiURL);
        if (response.status !== 200) {
            throw new Error(
                `failed to fetch url=${apiURL}, text=${await response.text()}`,
            );
        }
        const data = await response.json();
        if (data.items.length === 0) {
            console.error(`channel not found: ${channelId}`);
        }

        if (channelId.startsWith("@")) {
            console.error(
                `channelId: ${channelId} -> ${data.items[0].id.channelId}`,
            );
        }

        channel.id = data.items[0].id.channelId;
        channel.title = data.items[0].snippet.title;
    } else {
        const apiURL =
            `${baseURL}/channels?part=snippet&id=${channelId}&key=${args.apikey}`;
        const response = await fetch(apiURL);
        if (response.status !== 200) {
            throw new Error(
                `failed to fetch url=${apiURL}, text=${await response.text()}`,
            );
        }
        const data = await response.json();
        if (data.items.length === 0) {
            console.error(`channel not found: ${channelId}`);
        }
        channel.title = data.items[0].snippet.title;
    }

    console.log("");
    console.log(
        `[${channel.title}](https://www.youtube.com/channel/${channel.id})`,
    );
    console.log("");

    const apiURL =
        `${baseURL}/search?part=snippet&channelId=${channel.id}&maxResults=${args.maxResults}&order=${args.order}&publishedAfter=${publishedAfter}&key=${args.apikey}`;
    const response = await fetch(`${apiURL}&key=${args.apikey}`);
    if (response.status !== 200) {
        throw new Error(
            `failed to fetch url=${apiURL}, text=${await response.text()}`,
        );
    }
    const data = await response.json();
    for (const item of data.items) {
        console.log(
            `- [${item.snippet.title}](https://www.youtube.com/watch?v=${item.id.videoId}) (${item.snippet.publishedAt})`,
        );
    }
    //  console.log(JSON.stringify(data, null, 2));
}
