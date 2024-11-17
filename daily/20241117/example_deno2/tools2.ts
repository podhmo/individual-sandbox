import * as esbuild from "npm:esbuild";
import { denoPlugins } from "jsr:@luca/esbuild-deno-loader";
import { parseArgs } from "jsr:@podhmo/with-help@0.4.0";


function PathReplacePlugin(options: { configPath?: string } = {}) {
    // deno.json
    interface Config {
        imports?: Record<string, string>;
    }

    // deno.lock
    interface LockConfig {
        specifiers?: Record<string, string>;
    }

    // TODO: find workspace
    // TODO: handling deno.lock
    // TODO: npm support

    let config: Config = { imports: {} }; // deno.json 
    if (options.configPath) {
        debug(`[DEBUG] load deno.jsonfrom ${options.configPath}`);
        config = JSON.parse(Deno.readTextFileSync(options.configPath))
        if (config.imports == undefined) {
            config.imports = {};
        }

        try {
            debug(`[DEBUG] load deno.lock from ${options.configPath}`);
            const lockConfig: LockConfig = JSON.parse(Deno.readTextFileSync(options.configPath.replace("deno.json", "deno.lock")));
            if (lockConfig.specifiers !== undefined) {
                for (const [alias, path] of Object.entries(config.imports)) {
                    const version = lockConfig.specifiers[path];
                    if (version) {
                        const parts = path.split("@")
                        // e.g. jsr:@std/collections@^1.0.9 ->  jsr:@std/collections@1.0.9 (from deno.lock)
                        config.imports[alias] = parts.slice(0, parts.length - 1).join("@") + `@${version}`;
                        debug(`[DEBUG] locked version ${alias} -> ${config.imports[alias]}`);
                    }
                }
            }
        } catch (e) {
            debug(`[WARN] no deno.lock found: ${e}`);
        }
    }

    return {
        name: "path-resolve-plugin",
        setup(build: esbuild.PluginBuild) {
            // local deno.json's imports
            for (const [alias, path] of Object.entries(config.imports ?? {})) {
                debug(`[DEBUG] setup resolve ${alias} -> ${path}`);
                const regexp = new RegExp(`^${alias}/`);
                build.onResolve({ filter: regexp }, (args: esbuild.OnResolveArgs): esbuild.OnResolveResult | null => {
                    debug(`[DEBUG] resolve ${args.path} -> ${path}`);
                    let replaced = args.path.replace(regexp, path + "/");
                    if(replaced.startsWith("jsr:")) {
                        replaced = replaced.replace("jsr:", "https://esm.sh/jsr/");
                    }
                    return { path: replaced, external: true }
                });
            }

            // jsr: -> https://esm.sh/jsr/
            debug("[DEBUG] setup resolve jsr: -> https://esm.sh/jsr/");
            build.onResolve({ filter: /^jsr:.+$/ }, (args: esbuild.OnResolveArgs): esbuild.OnResolveResult | null => {
                debug(`[DEBUG] resolve ${args.path}`);
                return { external: true, path: args.path.replace(/^jsr:/, "https://esm.sh/jsr/") }
            });
        }
    }
}

const args = parseArgs(Deno.args, {
    string: ["src", "dst", "config"],
    boolean: ["debug"],
    required: ["src", "dst"],
})

function debug(msg: string) {
    if (args.debug) {
        console.error(msg);
    }
}

const result = await esbuild.build({
    plugins: [PathReplacePlugin({ configPath: args.config }), ...denoPlugins({
        loader: "native",
    })],
    entryPoints: [args.src],
    outfile: args.dst,
    bundle: true,
    format: "esm",
})

console.log(`writing ${result.outputFiles}`);
await esbuild.stop(); 