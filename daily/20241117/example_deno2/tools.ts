import { parseArgs } from "jsr:@podhmo/with-help@0.4.0"

import { Project } from "jsr:@ts-morph/ts-morph@24.0.0";
import { resolve, dirname } from "jsr:@std/path@1.0.8";

class ImportResolver {
    constructor(private importMap: Record<string, string>) { }

    resolveImportPath(importPath: string): string {
        if (importPath.startsWith("npm:")) {
            const packageName = importPath.slice(4);
            return `https://esm.sh/${packageName}`;
        }
        if (importPath.startsWith("jsr:")) {
            const packageName = importPath.slice(4);
            return `https://esm.sh/${packageName}`;
        }
        if (this.importMap[importPath]) {
            return this.importMap[importPath];
        }
        return importPath;
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
        required: ["src", "dst"],
    })

    const entryFile = args.src;
    const outputDir = args.dst;

    let importMap = {};
    if (args.config) {
        importMap = JSON.parse(Deno.readTextFileSync(args.config)).imports; // 手抜き
    }

    const project = new Project();
    const visitedFiles = new Set<string>();


    // エントリポイントから変換開始
    const walker = new Walker(new ImportResolver(importMap), visitedFiles);
    walker.WalkFile(entryFile, project);

    // 変換したファイルを出力
    Deno.mkdirSync(outputDir, { recursive: true });
    project.getSourceFiles().forEach((sourceFile) => {
        const filePath = sourceFile.getFilePath();
        const outputFilePath = resolve(outputDir, filePath);
        Deno.mkdirSync(dirname(outputFilePath), { recursive: true });
        Deno.writeTextFileSync(outputFilePath, sourceFile.getFullText());
    });
}

if (import.meta.main) {
    await main();
}


