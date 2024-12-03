import { parseArgs } from "jsr:@podhmo/with-help@0.5.2";

async function main() {
    const args = parseArgs(Deno.args, {
        description: "gistにpostしてそのURLを文字列として利用したい",
        string: [
            "filename",
            "content",
        ],
        required: ["filename", "content"],
        boolean: ["public", "debug"],
        default: {
            filename: "README.md",
        },
        envvar: {
            debug: "DEBUG",
        },
    });

    const cmdArgs: string[] = ["gist", "create", "--filename", args.filename];
    if (args.public) {
        cmdArgs.push("--public");
    }

    const command = new Deno.Command("gh", {
        args: cmdArgs,
        stdin: "piped",
        stdout: "piped",
        stderr: "inherit",
    });

    const process = command.spawn();
    const writer = process.stdin.getWriter();
    await writer.write(new TextEncoder().encode(args.content));
    await writer.close();

    const { code, stdout } = await process.output();

    if (code !== 0) {
        console.error("failed to create a gist");
        return;
    }

    const lines = new TextDecoder().decode(stdout).trim().split("\n");
    const url = lines[lines.length - 1];

    if (args.debug) {
        console.dir({ stdout: lines });
    }
    console.log("gist url:", url);
}

if (import.meta.main) {
    await main();
}
