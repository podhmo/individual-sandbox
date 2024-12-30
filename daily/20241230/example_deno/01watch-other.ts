import { debounce } from "jsr:@std/async@1.0.9/debounce";

// deno run -A 01watch-other.ts

const watcher = Deno.watchFs(".", { recursive: true });
const debounced = debounce((ev: Deno.FsEvent) => {
    console.log(`debounced: %o`, ev);
}, 200);

for await (const ev of watcher) {
    debounced(ev);
}

// Output:
//
// debounced: [Object: null prototype] {
//     kind: "access",
//     paths: [
//       "/home/po/ghq/github.com/podhmo/individual-sandbox/daily/20241230/example_deno/./00.html"
//     ],
//     flag: null
//   }
