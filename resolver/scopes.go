package resolver

import (
	"embed"
	"fmt"
	"path/filepath"
	"strings"

	"github.com/masp/ertylang/ast"
	"github.com/masp/ertylang/parser"
	"github.com/masp/ertylang/types"
)

type ScopeLevel int

const (
	ScopeUniverse ScopeLevel = iota
	ScopeModule
	ScopeFunc
)

// Scope is a symbol table for a given Scope, for example a function
// body or an if-statement. Every Scope can only have one declaration per
// name, where duplicate names override previous ones. Scope is only meant to be
// used during resolution.
type Scope struct {
	Level   ScopeLevel
	Outer   *Scope
	Symbols map[string]*types.Decl
	currFn  *ast.FuncDecl // ptr to current function body (nil if scope is not defined by func)
}

func NewScope(outer *Scope) *Scope {
	level := ScopeUniverse
	if outer != nil {
		if level < ScopeFunc {
			level = outer.Level + 1
		} else {
			level = outer.Level
		}
	}
	return &Scope{Outer: outer, Symbols: make(map[string]*types.Decl), Level: level}
}

func (s *Scope) Lookup(name string) *types.Decl {
	if obj, ok := s.Symbols[name]; ok {
		return obj
	}

	if s.Outer != nil {
		return s.Outer.Lookup(name)
	}
	return nil
}

func (s *Scope) CurrentFunc() *ast.FuncDecl {
	if s.currFn != nil {
		return s.currFn
	}
	if s.Outer != nil {
		return s.Outer.CurrentFunc()
	}
	return nil
}

func (s *Scope) Insert(name string, decl *types.Decl) (found *types.Decl) {
	s.Symbols[name] = decl
	return decl
}

var (
	Universe    *Scope
	builtInDecl *ast.BadDecl // a token value indicating the value is a built-in

	BuiltinsImporter Importer
	//go:embed preloaded/*
	preloaded embed.FS
)

func init() {
	Universe = NewScope(nil)
	for id, typ := range types.Builtins {
		Universe.Insert(id, &types.Decl{
			Type:     typ,
			RefersTo: builtInDecl,
		})
	}

	imp := &CachedImporter{LoadedModules: make(map[string]*ast.Module)}
	preloadPaths, err := preloaded.ReadDir("preloaded")
	if err != nil {
		panic(err)
	}
	for _, pre := range preloadPaths {
		if pre.IsDir() {
			continue
		}

		if strings.HasSuffix(pre.Name(), ".d.ert") {
			recursiveResolve(imp, strings.TrimSuffix(pre.Name(), ".d.ert"))
		}
	}
	BuiltinsImporter = imp
}

func recursiveResolve(imp *CachedImporter, moduleName string) {
	src, err := preloaded.ReadFile(filepath.Join("preloaded", moduleName+".d.ert"))
	if err != nil {
		panic(err)
	}
	mod, err := parser.ParseModule(moduleName, src, &parser.Options{DeclarationOnly: true})
	if err != nil {
		panic(fmt.Errorf("compile preloaded/%s: %w", moduleName+".d.ert", err))
	}

	// before we resolve, recursively resolve all imports if they don't exist
	for _, dep := range mod.Imports {
		_, err := imp.Load(dep.Path.Value)
		if err == ErrModuleMissing {
			recursiveResolve(imp, dep.Path.Value)
		} else if err != nil {
			panic(err)
		}
	}

	err = ResolveModule(mod, &Config{Importer: imp})
	if err != nil {
		panic(fmt.Errorf("resolve preloaded/%s: %w", moduleName+".d.ert", err))
	}
	imp.Add(moduleName, mod)
}
