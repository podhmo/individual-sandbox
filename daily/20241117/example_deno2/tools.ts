import { parseArgs } from "jsr:@podhmo/with-help@0.4.0"

import { Project } from "jsr:@ts-morph/ts-morph@24.0.0";
import { resolve, dirname, relative } from "jsr:@std/path@1.0.8";


// import pathをHTML用のimportmapに変換するための状態を保持するクラス
// 例えば
// import {chunk} from "jsr:@std/collections@1.0.0/chunk";
// は
// import {chunk} from "@collections@1.0.0/chunk"; に変換され
// HTML用のimportmapには
// "@std/collections": "https://esm.sh/jsr/@std/collections@1.0.0/chunk",
// が追加される
class ImportResolver {
    constructor(
        private importMap: Record<string, string> = {}, // deno.json#/imports
        private urlMap: Record<string, string> = {}, // alias -> url
    ) { }

    // jsr:@std/collections@1.0.0/chunk -> @std/collections@1.0.0/chunk
    resolveAlias(path: string): string {
        if (path.startsWith("jsr:")) {
            const packageName = path.slice(4);
            this.urlMap[packageName] = `https://esm.sh/jsr/${packageName}`;
            return packageName;
        }
        return path
    }

    resolveImportPath(importPath: string): string {
        if (this.importMap[importPath]) {
            importPath = this.importMap[importPath];
        }

        const alias = this.resolveAlias(importPath);
        return alias;
    }
}

class Walker {
    constructor(
        private importResolver: ImportResolver,
        private visited: Set<string> = new Set<string>(),
    ) { }

    // ファイルを変換する関数
    WalkFile(filePath: string, project: Project): void {
        if (this.visited.has(filePath)) {
            return  // すでに処理済みのファイルをスキップ
        }
        this.visited.add(filePath);
        console.error(`read ${filePath}`);

        const sourceFile = project.addSourceFileAtPath(filePath);

        // インポート文を置き換え
        sourceFile.getImportDeclarations().forEach((importDecl) => {
            const moduleSpecifier = importDecl.getModuleSpecifierValue();
            const resolvedPath = this.importResolver.resolveImportPath(moduleSpecifier);
            if (moduleSpecifier !== resolvedPath) {
                importDecl.setModuleSpecifier(resolvedPath);
            }
        });

        // 再帰的に依存ファイルを処理
        sourceFile.getImportDeclarations().forEach((importDecl) => {
            const moduleSpecifier = importDecl.getModuleSpecifierValue();
            if (moduleSpecifier.startsWith(".") || moduleSpecifier.startsWith("/")) {
                const resolvedPath = resolve(dirname(filePath), moduleSpecifier);
                this.WalkFile(resolvedPath, project); // 再帰的に処理
            }
        });
    }
}


function main() {
    const args = parseArgs(Deno.args, {
        string: ["src", "dst", "config"],
        boolean: ["dry-run"],
        required: ["src", "dst"],
    })

    const entryFile = args.src;
    const outputDir = args.dst;

    let importMap = {};
    if (args.config) {
        importMap = JSON.parse(Deno.readTextFileSync(args.config)).imports; // 手抜き
    }
    const urlMap = {};

    const project = new Project();
    const visitedFiles = new Set<string>();


    // エントリポイントから変換開始
    const walker = new Walker(new ImportResolver(importMap, urlMap), visitedFiles);
    walker.WalkFile(entryFile, project);

    // 変換したファイルを出力
    const absSourceDir = resolve(entryFile, "..");

    project.getSourceFiles().forEach((sourceFile) => {
        const filePath = sourceFile.getFilePath();
        const outputFilePath = filePath.replace(absSourceDir, outputDir)
            .replace("/src_", "/") // この辺はgistに共有する用の変換
            .replace("dst/", "dst_")
            ;

        // console.dir({ filePath, outputFilePath, outputDir }, { depth: 3 });
        console.error(`write ${outputFilePath} (dry-run=${args["dry-run"]})`);
        if (args["dry-run"]) {
            return;
        }
        Deno.mkdirSync(dirname(outputFilePath), { recursive: true });
        Deno.writeTextFileSync(outputFilePath, sourceFile.getFullText());
        
        Deno.writeTextFileSync(`${outputFilePath}.map`, JSON.stringify({imports: urlMap}, null, 2));
    });
}

if (import.meta.main) {
    await main();
}


