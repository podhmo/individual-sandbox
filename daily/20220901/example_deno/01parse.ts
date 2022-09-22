import { parse } from "https://deno.land/std@0.153.0/encoding/yaml.ts";

const text = await Deno.readTextFile("./data.yaml");
const data = parse(text);

console.log(data);

// // ordered?
// {
//     definitions: {
//       X: { type: "object" },
//       Person: { properties: { name: [Object], age: [Object] } },
//       Y: { type: "object" },
//       People: { type: "array", items: { "$ref": "#/definitions/Person" } }
//     }
// }
