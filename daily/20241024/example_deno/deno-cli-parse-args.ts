import { parseArgs } from "jsr:@std/cli/parse-args";

const flags = parseArgs(Deno.args, {
  boolean: ["color"],
  string: ["version"],
  default: { color: true },
  negatable: ["color"],
});

// deno run deno-cli-parse-args.ts --color=false --version=1.0.0
// deno run deno-cli-parse-args.ts --no-color --version=1.0.0
//
// hmm
// deno run deno-cli-parse-args.ts --colr=false --versionn=1.0.0 x y z
console.dir(flags, { depth: null});
