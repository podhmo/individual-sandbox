import { parseArgs } from "node:util";

const { values, positionals } = parseArgs({
    options: {
        "no-color": { type: 'boolean', default: false },
        version: { type: 'string' }
    },
    allowPositionals: true
});
const flags = { ...values, color: !values["no-color"] };

// deno run node-util-parse-args.ts --version=1.0.0
// deno run node-util-parse-args.ts --no-color --version=1.0.0
//
// hmm
// deno run node-util-parse-args.ts --colr=false --versionn=1.0.0 x y z
console.dir(flags, { depth: null });
console.dir(positionals, { depth: null });