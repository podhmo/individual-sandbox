import * as jsonc from "jsr:@std/jsonc@1.0.1";
import { parseArgs } from "jsr:@podhmo/with-help@0.4.0";

type Config = {
    name: string;
    nickname?: string;
};

// deno run --allow-read main.ts --config config.jsonc
const args = parseArgs(Deno.args, {
    string: ["config"],
    required: ["config"],
});
const rawConfig = jsonc.parse(await Deno.readTextFile(args.config));
const config = JSON.parse(JSON.stringify(rawConfig)) as Config;

console.log("load: config");
console.dir(config, { depth: null }); // { name: "foo", nickname: "F" }
