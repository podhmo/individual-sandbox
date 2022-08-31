// https://deno.land/std@0.153.0/encoding/README.md?source=#yaml

import {
    parse,
    stringify,
} from "https://deno.land/std@0.153.0/encoding/yaml.ts";

const data = parse(`
foo: bar
baz:
- qux
- quux
`);
console.log(data);
// => { foo: "bar", baz: [ "qux", "quux" ] }

const yaml = stringify({ foo: "bar", baz: ["qux", "quux"] });
console.log(yaml);
