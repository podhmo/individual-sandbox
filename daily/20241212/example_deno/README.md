# bsky

## 00 password input

パスワードとidentifierを入力して表示する

## 01 login bluesky

入力したpasswordなどを使ってログインする
📝 passwordはmaskしたいかも？(defaultでmaskする？)

テキトーに.envに保存することにした.configファイルがほしいかも？

## 02 auth subcommandを作る

そういえばサブコマンドを作る例を書いていたな。
https://github.com/podhmo/with-help/blob/main/examples/build-subcommands.ts

サブコマンドを書くのはだるいね。。

```ts
interface BaseOptions {
    debug: boolean;
}

async function main() {
    const baseOptions = parseArgs(Deno.args, {
        name: "bsky",
        boolean: ["debug"],
        stopEarly: true, // for subcommand
        footer: `
Available Commands:
  auth: authentication for bluesky`,
    });

    const args = baseOptions._;
    if (args.length === 0) {
        console.error("%cneed command", "color: red; font-weight: bold");
        printHelp(baseOptions);
        return;
    }
    switch (args[0]) {
        case "auth":
            await authCommand(args.slice(1), baseOptions);
            break;
        default:
            console.error(
                `%cunknown command: ${args[0]}`,
                "color: red; font-weight: bold",
            );
            printHelp(baseOptions);
            break;
    }
}
```

## 03 auth statusを作る

auth statusと言いつつauth doctorのようなものかも？



# references

- https://jsr.io/@std/cli/doc/prompt-secret/~/promptSecret
- [gist -- bskyにスレッドで投稿したい](https://gist.github.com/podhmo/19a4e189dd5c9a8d3af871139c51b9fe)