const doc = {"x": {"name": "foo"}}
const patches = [
    {
        "op": "add",
        "path": "",
        "value": "x"
    },
]

const jiff = require("jiff")
console.log(JSON.stringify(jiff.patch(patches, doc), null, 2));
