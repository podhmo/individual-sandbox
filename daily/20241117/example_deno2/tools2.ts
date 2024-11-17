import * as esbuild from "npm:esbuild";
import { denoPlugins } from "jsr:@luca/esbuild-deno-loader";
import { parseArgs } from "jsr:@podhmo/with-help@0.4.0";

function PathResolvePlugin() {
    return {
        name: "path-resolve-plugin",
        setup(build: esbuild.PluginBuild) {
            build.onResolve({ filter: /^jsr:.+$/ }, (args: esbuild.OnResolveArgs): esbuild.OnResolveResult | null => {
                // console.log("path-resolve-plugin", JSON.stringify(args));
                return {external: true, path: args.path.replace(/^jsr:/, "https://esm.sh/jsr/")}
            })
        }
    }
}

const args = parseArgs(Deno.args, {
    string: ["src", "dst", "config"],
    required: ["src", "dst"],
})

const result = await esbuild.build({
    plugins: [PathResolvePlugin(), ...denoPlugins({
        loader: "native",
        configPath: args.config,
    })], 
    entryPoints: [args.src],
    outfile: args.dst,
    bundle: true, 
    format: "esm",
})

console.log(`writing ${result.outputFiles}`);
await esbuild.stop(); 