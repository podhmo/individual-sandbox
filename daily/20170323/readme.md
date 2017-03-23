# node.jsでCLI作るのに良いのがわかんない

## hmm

- jsonfile
- minimist
- mongodb

## hello

```js
const parseArgs = require("minimist");
const argv = parseArgs(process.argv.slice(2), {
  string: [
    "config"
  ],
  alias: {
    c: "config"
  }
})
if (!argv.config){
  console.log(`
hello
  -c : conf path (example: conf/monitoring-local.json)
`)
  process.exit(1)
}

async function run(){
  let db = await MongoClient.connect(url)
  let cursor = await db.collection("values");
  console.log(await cursor.find().toArray());
  console.log(await cursor.find().count());
  await db.close();
  return db;
}

run()
.catch((err) => {
  console.error(err);
})

```

## node.js 7.6からasync/awaitつかえるっぽい

使えるっぽい
