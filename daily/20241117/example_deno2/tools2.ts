import * as esbuild from "npm:esbuild";
import { denoPlugins } from "jsr:@luca/esbuild-deno-loader";
import { parseArgs } from "jsr:@podhmo/with-help@0.4.0";


function PathReplacePlugin(options: { configPath?: string } = {}) {
    // deno.json
    interface Config {
        imports: Record<string, string>;
        specifiers: Record<string, string>;
    }

    // deno.lock
    interface LockConfig {
        specifiers: Record<string, string>;
    }

    // TODO: find workspace
    // TODO: handling deno.lock
    // TODO: npm support

    let config: Config = { imports: {}, specifiers: {} }; // deno.json 
    if (options.configPath) {
        debug(`[DEBUG] load deno.json from ${options.configPath}`);
        config = JSON.parse(Deno.readTextFileSync(options.configPath))
        if (config.imports == undefined) {
            config.imports = {};
        }
        if (config.specifiers == undefined) {
            config.specifiers = {};
        }

        try {
            debug(`[DEBUG] load deno.lock from ${options.configPath}`);
            const lockConfig: LockConfig = JSON.parse(Deno.readTextFileSync(options.configPath.replace("deno.json", "deno.lock")));
            config.specifiers = lockConfig.specifiers;
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

                    // jsr: -> https://esm.sh/jsr/
                    if (replaced.startsWith("jsr:")) {
                        replaced = replaced.replace("jsr:", "https://esm.sh/jsr/");
                    }

                    // npm: -> https://esm.sh/
                    if (replaced.startsWith("npm:")) {
                        replaced = replaced.replace("npm:", "https://esm.sh/");
                    }
                    return { path: replaced, external: true }
                });
            }

            // jsr: -> https://esm.sh/jsr/
            debug("[DEBUG] setup resolve jsr: -> https://esm.sh/jsr/");
            build.onResolve({ filter: /^jsr:.+$/ }, (args: esbuild.OnResolveArgs): esbuild.OnResolveResult | null => {
                debug(`[DEBUG] resolve ${args.path}`);
                let replaced = args.path.replace(/^jsr:/, "https://esm.sh/jsr/");
                const version = config.specifiers[args.path + "@*"];
                if (version) {
                    replaced = `${replaced}@${version}`;
                }
                return { external: true, path: replaced };
            });

            // npm: -> https://esm.sh/
            debug("[DEBUG] setup resolve npm: -> https://esm.sh/");
            build.onResolve({ filter: /^npm:.+$/ }, (args: esbuild.OnResolveArgs): esbuild.OnResolveResult | null => {
                debug(`[DEBUG] resolve ${args.path}`);
                let replaced = args.path.replace(/^npm:/, "https://esm.sh/");
                const version = config.specifiers[args.path + "@*"];
                if (version) {
                    replaced = `${replaced}@${version}`;
                }
                return { external: true, path: replaced };
            });

            // // TODO: generate html importmap
            // build.onEnd(() => {
            // });
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