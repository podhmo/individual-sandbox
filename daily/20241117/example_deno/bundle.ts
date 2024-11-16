import * as esbuild from "npm:esbuild";
import { denoPlugins } from "jsr:@luca/esbuild-deno-loader";
import {parseArgs} from "jsr:@podhmo/with-help@0.4.0";

const args = parseArgs(Deno.args, {
    string: ["src", "dst", "config"],
    required: ["src", "dst"],
})

const result = await esbuild.build({
    plugins: [...denoPlugins({
        loader: "native",
        configPath: args.config,
    })], // resolver, loader
    entryPoints: [args.src],
    outfile: args.dst,
    bundle: true,  // falseにするとどういう効果があるんだろ？
    format: "esm",
})

console.log(`writing ${result.outputFiles}`);
await esbuild.stop(); // 自動で呼んでくれないんだろうか？