const doc = {"name": "foo"}
const patches = [
    {
        "op": "copy",
        "path": "/person",
        "from": "/"
    },
]

const jiff = require("jiff")
console.log(JSON.stringify(jiff.patch(patches, doc), null, 2));
