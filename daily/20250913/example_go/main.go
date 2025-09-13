// main.go generates Go files containing only the signatures of the standard library.
package main

import (
	"bytes"
	"flag"
	"fmt"
	"go/ast"
	"go/build"
	"go/format"
	"go/parser"
	"go/printer"
	"go/token"
	"log/slog"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

func main() {
	// Define and parse command-line flags.
	outputDir := flag.String("output", "stdlib", "The output directory for the generated Go files.")
	debug := flag.Bool("debug", false, "Enable debug logging.")
	flag.Parse()

	// Setup the logger.
	logLevel := new(slog.LevelVar)
	if *debug {
		logLevel.Set(slog.LevelDebug)
	}
	logger := slog.New(slog.NewTextHandler(os.Stderr, &slog.HandlerOptions{Level: logLevel}))

	// Ensure the output directory exists.
	if err := os.MkdirAll(*outputDir, 0755); err != nil {
		logger.Error("Failed to create output directory", "path", *outputDir, "error", err)
		os.Exit(1)
	}
	logger.Info("Output directory prepared", "path", *outputDir)

	// Get the list of standard library packages.
	logger.Info("Retrieving standard library package list...")
	cmd := exec.Command("go", "list", "std")
	out, err := cmd.Output()
	if err != nil {
		logger.Error("Failed to list standard library packages", "error", err)
		os.Exit(1)
	}

	packages := strings.Fields(string(out))
	logger.Info(fmt.Sprintf("Found %d standard library packages", len(packages)))

	// Process each package.
	for _, pkgPath := range packages {
		// Skip any package whose path contains an "internal" component.
		// Also skip vendor and cmd packages.
		isInternal := false
		for _, part := range strings.Split(pkgPath, "/") {
			if part == "internal" {
				isInternal = true
				break
			}
		}

		if isInternal || strings.Contains(pkgPath, "vendor/") || strings.HasPrefix(pkgPath, "cmd/") {
			logger.Debug("Skipping internal/vendor/cmd package", "package", pkgPath)
			continue
		}

		if err := processPackage(pkgPath, *outputDir, logger); err != nil {
			logger.Error("Failed to process package", "package", pkgPath, "error", err)
		}
	}

	logger.Info("Signature generation complete.")
}

// processPackage parses a package, transforms its AST, and writes the result to a file.
func processPackage(pkgPath, outputDir string, logger *slog.Logger) error {
	logger.Info("Processing package", "package", pkgPath)

	// Find the file system path for the given package import path.
	buildPkg, err := build.Import(pkgPath, "", 0)
	if err != nil {
		if _, ok := err.(*build.NoGoError); ok {
			logger.Debug("Skipping package with no Go source files", "package", pkgPath)
			return nil
		}
		return fmt.Errorf("could not find package %s: %w", pkgPath, err)
	}
	pkgDir := buildPkg.Dir

	// Parse non-test Go files in the package directory.
	fset := token.NewFileSet()
	pkgs, err := parser.ParseDir(fset, pkgDir, func(fi os.FileInfo) bool {
		return !strings.HasSuffix(fi.Name(), "_test.go")
	}, 0)

	if err != nil {
		return fmt.Errorf("could not parse directory %s for package %s: %w", pkgDir, pkgPath, err)
	}

	// Since we are filtering out _test.go files, there should only be one package.
	var astPkg *ast.Package
	for _, p := range pkgs {
		astPkg = p
		break
	}
	if astPkg == nil {
		// This can happen if a directory contains only _test.go files.
		logger.Debug("No non-test package found", "package", pkgPath)
		return nil
	}

	// Merge all files of the package into a single AST file, filtering unexported declarations.
	mergedFile := mergeAndFilterPackageFiles(astPkg, logger)

	// Inspect the AST and modify function declarations.
	ast.Inspect(mergedFile, func(n ast.Node) bool {
		fn, ok := n.(*ast.FuncDecl)
		if ok && fn.Body != nil {
			rewriteFuncBody(fn)
		}
		return true
	})

	// Prepare the output file path.
	targetPath := filepath.Join(outputDir, pkgPath)
	if err := os.MkdirAll(filepath.Dir(targetPath), 0755); err != nil {
		return fmt.Errorf("could not create parent directory for %s: %w", targetPath, err)
	}
	outFile, err := os.Create(targetPath + ".go")
	if err != nil {
		return fmt.Errorf("could not create output file for %s: %w", pkgPath, err)
	}
	defer outFile.Close()

	// Print the modified AST to a buffer.
	var buf bytes.Buffer
	cfg := printer.Config{Mode: printer.TabIndent, Tabwidth: 8}
	if err := cfg.Fprint(&buf, fset, mergedFile); err != nil {
		return fmt.Errorf("could not print AST for %s: %w", pkgPath, err)
	}

	// Format the generated code.
	formatted, err := format.Source(buf.Bytes())
	if err != nil {
		logger.Warn("Could not format generated code, writing unformatted version", "package", pkgPath, "error", err)
		if _, writeErr := outFile.Write(buf.Bytes()); writeErr != nil {
			return fmt.Errorf("could not write unformatted source for %s: %w", pkgPath, writeErr)
		}
		return fmt.Errorf("could not format source for %s: %w", pkgPath, err)
	}

	// Write the formatted code to the file.
	if _, err := outFile.Write(formatted); err != nil {
		return fmt.Errorf("could not write to output file for %s: %w", pkgPath, err)
	}

	logger.Debug("Successfully wrote signature file", "package", pkgPath, "path", outFile.Name())
	return nil
}

// mergeAndFilterPackageFiles combines all ast.File in a package into a single ast.File,
// keeping only exported and non-test declarations.
func mergeAndFilterPackageFiles(pkg *ast.Package, logger *slog.Logger) *ast.File {
	merged := &ast.File{
		Name:  ast.NewIdent(pkg.Name),
		Decls: []ast.Decl{},
	}
	imports := make(map[string]*ast.ImportSpec)

	for _, file := range pkg.Files {
		// Collect unique imports from each file.
		for _, spec := range file.Imports {
			path := spec.Path.Value
			if _, ok := imports[path]; !ok {
				imports[path] = spec
			}
		}

		// Process and filter declarations.
		for _, decl := range file.Decls {
			switch d := decl.(type) {
			case *ast.FuncDecl:
				// Keep function if it's exported and not a test/benchmark/example.
				name := d.Name.Name
				if d.Name.IsExported() &&
					!strings.HasPrefix(name, "Test") &&
					!strings.HasPrefix(name, "Benchmark") &&
					!strings.HasPrefix(name, "Example") &&
					!strings.HasPrefix(name, "Fuzz") {
					merged.Decls = append(merged.Decls, d)
				}
			case *ast.GenDecl:
				// For general declarations (import, const, var, type), filter specs.
				if d.Tok == token.IMPORT {
					continue
				}

				filteredSpecs := []ast.Spec{}
				for _, spec := range d.Specs {
					switch s := spec.(type) {
					case *ast.TypeSpec:
						if s.Name.IsExported() {
							filteredSpecs = append(filteredSpecs, s)
						}
					case *ast.ValueSpec:
						allExported := true
						for _, name := range s.Names {
							if !name.IsExported() {
								allExported = false
								break
							}
						}
						if allExported {
							filteredSpecs = append(filteredSpecs, s)
						}
					}
				}

				if len(filteredSpecs) > 0 {
					newDecl := &ast.GenDecl{
						Doc:    d.Doc,
						Tok:    d.Tok,
						Lparen: d.Lparen,
						Specs:  filteredSpecs,
						Rparen: d.Rparen,
					}
					merged.Decls = append(merged.Decls, newDecl)
				}
			}
		}
	}

	// Create a new import declaration for all unique imports.
	if len(imports) > 0 {
		importDecl := &ast.GenDecl{
			Tok:    token.IMPORT,
			Lparen: 1,
			Specs:  []ast.Spec{},
		}
		for _, spec := range imports {
			importDecl.Specs = append(importDecl.Specs, spec)
		}
		merged.Decls = append([]ast.Decl{importDecl}, merged.Decls...)
	}

	return merged
}

// rewriteFuncBody replaces the body of a function with a panic statement.
func rewriteFuncBody(fn *ast.FuncDecl) {
	if fn.Type.Results == nil || len(fn.Type.Results.List) == 0 {
		fn.Body = &ast.BlockStmt{}
		return
	}

	panicCall := &ast.CallExpr{
		Fun: &ast.Ident{Name: "panic"},
		Args: []ast.Expr{
			&ast.BasicLit{
				Kind:  token.STRING,
				Value: `"not implemented"`,
			},
		},
	}

	fn.Body = &ast.BlockStmt{
		List: []ast.Stmt{
			&ast.ExprStmt{X: panicCall},
		},
	}
}