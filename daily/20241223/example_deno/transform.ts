import * as esbuild from "npm:esbuild@0.24.2";
import * as jsonc from "jsr:@std/jsonc@1.0.1";
import { type Project } from "jsr:@ts-morph/ts-morph@24.0.0";

/**
 esbuild plugin for rewriting deno's original import path to esm.sh URL
*/
export async function PathReplacePlugin(
  options: { denoConfigPath?: string; debug: boolean } = { debug: false },
): Promise<esbuild.Plugin> {
  // deno.json
  interface Config {
    imports: Record<string, string>;
    specifiers: Record<string, string>;
  }

  // deno.lock
  interface LockConfig {
    specifiers: Record<string, string>;
  }

  const debug = options.debug ? console.error : () => {};

  let config: Config = { imports: {}, specifiers: {} }; // deno.json
  if (options.denoConfigPath) {
    debug(`[DEBUG] load deno.json from ${options.denoConfigPath}`);
    if (options.denoConfigPath.endsWith(".jsonc")) {
      const text = await Deno.readTextFile(options.denoConfigPath);
      config = JSON.parse(JSON.stringify(jsonc.parse(text)));
    } else {
      const text = await Deno.readTextFile(options.denoConfigPath);
      config = JSON.parse(text);
    }

    if (config.imports == undefined) {
      config.imports = {};
    }
    if (config.specifiers == undefined) {
      config.specifiers = {};
    }
    try {
      debug(`[DEBUG] load deno.lock from ${options.denoConfigPath}`);
      const lockConfig: LockConfig = JSON.parse(
        await Deno.readTextFile(
          options.denoConfigPath.replace("deno.json", "deno.lock"),
        ),
      );
      config.specifiers = lockConfig.specifiers;
      if (lockConfig.specifiers !== undefined) {
        for (const [alias, path] of Object.entries(config.imports)) {
          const version = lockConfig.specifiers[path];
          if (version) {
            const parts = path.split("@");
            // e.g. jsr:@std/collections@^1.0.9 ->  jsr:@std/collections@1.0.9 (from deno.lock)
            config.imports[alias] = parts.slice(0, parts.length - 1).join("@") +
              `@${version}`;
            debug(
              `[DEBUG] locked version ${alias} -> ${config.imports[alias]}`,
            );
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
        const regexp = new RegExp(`^${alias}(/|$)`);
        build.onResolve(
          { filter: regexp },
          (args: esbuild.OnResolveArgs): esbuild.OnResolveResult | null => {
            debug(`[DEBUG] resolve ${args.path} -> ${path}`);
            let replaced = args.path.replace(regexp, path);

            // jsr: -> https://esm.sh/jsr/
            if (replaced.startsWith("jsr:")) {
              replaced = replaced.replace("jsr:", "https://esm.sh/jsr/");
            }

            // npm: -> https://esm.sh/
            if (replaced.startsWith("npm:")) {
              replaced = replaced.replace("npm:", "https://esm.sh/");
            }
            return { path: replaced, external: true };
          },
        );
      }

      // jsr: -> https://esm.sh/jsr/
      debug("[DEBUG] setup resolve jsr: -> https://esm.sh/jsr/");
      build.onResolve(
        { filter: /^jsr:/ },
        (args: esbuild.OnResolveArgs): esbuild.OnResolveResult | null => {
          debug(`[DEBUG] resolve ${args.path}`);
          let replaced = args.path.replace(/^jsr:/, "https://esm.sh/jsr/");
          const version = config.specifiers[args.path + "@*"];
          if (version) {
            replaced = `${replaced}@${version}`;
          }
          return { external: true, path: replaced };
        },
      );

      // npm: -> https://esm.sh/
      debug("[DEBUG] setup resolve npm: -> https://esm.sh/");
      build.onResolve(
        { filter: /^npm:/ },
        (args: esbuild.OnResolveArgs): esbuild.OnResolveResult | null => {
          debug(`[DEBUG] resolve ${args.path}`);
          let replaced = args.path.replace(/^npm:/, "https://esm.sh/");
          const version = config.specifiers[args.path + "@*"];
          if (version) {
            replaced = `${replaced}@${version}`;
          }
          return { external: true, path: replaced };
        },
      );

      // build.onLoad(
      //   { filter: /.*/, namespace: "file" },
      //   (args: esbuild.OnLoadArgs) => {
      //     debug(`[DEBUG] load ${args.path}`);
      //     return;
      //   },
      // );

      // // TODO: generate html importmap
      // build.onEnd(() => {
      // });
    },
  };
}

/** for optimization */
export function collectImportsFromCode(
  project: Project,
  code: string,
): Record<string, { named: Set<string>; namespace: Set<string> }> {
  const sourceFile = project.createSourceFile("temp.ts", code);
  const importDeclarations = sourceFile.getImportDeclarations();

  // import-declartion is like:
  const r: Record<string, { named: Set<string>; namespace: Set<string> }> = {};
  for (const importDeclaration of importDeclarations) {
    const moduleSpecifier = importDeclaration.getModuleSpecifierValue();

    if (r[moduleSpecifier] === undefined) {
      r[moduleSpecifier] = {
        named: new Set<string>(),
        namespace: new Set<string>(),
      };
    }
    const info = r[moduleSpecifier];
    // import {<named imports>} from <module specifier value>
    for (const namedImport of importDeclaration.getNamedImports()) {
      info.named.add(namedImport.getName());
    }

    // import * as all from <module specifier value>
    const namespaceImport = importDeclaration.getNamespaceImport();
    if (namespaceImport) {
      info.namespace.add(namespaceImport.getText());
    }
  }

  return r;
}

export async function transform(
  options: {
    filename: string;
    debug: boolean;
    denoConfigPath?: string;
  },
): Promise<string> {
  const plugins: esbuild.Plugin[] = [
    await PathReplacePlugin(options),
  ];
  const b: esbuild.BuildResult = await esbuild.build({
    // inject, alias
    entryPoints: [options.filename],
    plugins: plugins,
    write: false,
    color: true,
    bundle: true, // need for resolve import
    logLevel: options.debug ? "debug" : "info",
    format: "esm",
  });
  if (b.outputFiles === undefined) {
    if (b.errors.length > 0) {
      throw new Error(b.errors[0].text);
    }
    throw new Error("no outputFiles");
  }
  return b.outputFiles[0].text;
}
