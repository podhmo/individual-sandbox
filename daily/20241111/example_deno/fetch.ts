import { parseArgs } from "jsr:@podhmo/with-help@0.4.0";

// const eventType: "completed" | "live" | "upcoming" = "completed"; // with type="video"
const type_: "video" | "channel" | "playlist" = "channel";
const orderCandidates = [
  "date",
  "rating",
  "relevance",
  "title",
  "videoCount",
  "viewCount",
] as const;

async function main() {
  // ...parseArgsみたいな形で一度スプレッドしてあげると型がvscode-denoの上で読みやすく表示されるけれど不毛っぽいきがする
  // [deno-ts] Type '{ query: string; apikey: string; maxResults: string; statistics: boolean; help: boolean; _: string[]; }'
  const args = {
    ...parseArgs(Deno.args, {
      description: "search youtube",
      string: ["query", "apikey", "maxResults", "order"],
      required: ["query", "apikey", "maxResults"],
      boolean: ["statistics"],
      negatable: ["statistics"],
      envvar: {
        apikey: "YOUTUBE_API_KEY",
      },
      default: {
        maxResults: "50",
        order: "date",
      },
      flagDescription: {
        "order":
          "(default: date) choices of [date, rating, relevance, title, videoCount, viewCount]",
      },
    }),
  };
  if (!orderCandidates.includes(args.order as typeof orderCandidates[number])) {
    console.error(`order must be one of ${orderCandidates.join(", ")}`);
    Deno.exit(1);
  }

  // https://developers.google.com/youtube/v3/docs/search/list
  // https://developers.google.com/youtube/v3/docs/channels/list

  const q = encodeURIComponent(args.query);
  const maxResults = parseInt(args.maxResults, 10);
  const baseURL = "https://www.googleapis.com/youtube/v3";
  const url =
    `${baseURL}/search?q=${q}&part=snippet&key=${args.apikey}&type=${type_}&maxResults=${maxResults}&order=${args.order}`;

  const response = await fetch(url);
  const data: Response<SearchItem> = await response.json();
  if (response.status !== 200) {
    throw new Error(data.error.message);
  }

  // TODO: handling nextPageToken
  console.error(JSON.stringify(
    {
      maxResults,
      nextPageToken: data.nextPageToken,
      totalResults: data.pageInfo.totalResults,
    },
  ));

  if (data.items) {
    for (const item of data.items) {
      const data = {
        channelId: item.snippet.channelId,
        title: item.snippet.title,
        channelTitle: item.snippet.channelTitle,
        publishedAt: item.snippet.publishedAt,
        statistics: {},
      };

      if (args.statistics) {
        const url =
          `${baseURL}/channels?part=statistics&id=${item.snippet.channelId}&key=${args.apikey}`;
        const response = await fetch(url);
        if (response.status !== 200) {
          throw new Error("failed to fetch statistics");
        }
        const subdata: Response<StatisticsItem> = await response.json();
        // console.log(JSON.stringify(data, null, 2));
        if (subdata.items.length === 0) {
          console.error("no statistics data");
        } else {
          data.statistics = subdata.items[0].statistics;
          if (subdata.items.length > 1) {
            console.error(
              `unexpected statistics data title=${data.title}, statics.length=${subdata.items.length}`,
            );
          }
        }
      }
      // console.log(JSON.stringify(data, null, 2));
      console.log(
        `"${data.channelId}", // ${data.title}[${data.statistics.videoCount}] @ ${data.statistics.subscriberCount} -- ${data.publishedAt}`,
      );
    }
  } else {
    console.log("[]");
  }
}

export interface Response<T> {
  kind: string;
  etag: string;
  nextPageToken: string;
  regionCode: string;
  pageInfo: {
    totalResults: number;
    resultsPerPage: number;
  };

  items: T[];
  error: {
    code: number;
    message: string;
  };
}

export interface SearchItem {
  kind: string;
  etag: string;
  id: {
    kind: string;
    channelId: string;
  };
  snippet: {
    publishedAt: string;
    channelId: string;
    title: string;
    description: string;
    thumbnails: {
      default: {
        url: string;
      };
      medium: {
        url: string;
      };
      high: {
        url: string;
      };
    };
    channelTitle: string;
    liveBroadcastContent: string;
    publishTime: string;
  };
}

export interface StatisticsItem {
  kind: string;
  etag: string;
  id: string;
  statistics: {
    viewCount: string;
    subscriberCount: string;
    hiddenSubscriberCount: boolean;
    videoCount: string;
  };
}

if (import.meta.main) {
  await main();
}
