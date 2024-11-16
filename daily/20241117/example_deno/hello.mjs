import {parseArgs} from "./with-help.dist.js";

const args = parseArgs(Deno.args, {
    description: "試しにbundleされたwith-helpを使ってみる",
    string: ["name"],
    required: ["name"],
    default: {name: "world"},
});

console.log(`Hello, ${args.name}!`);