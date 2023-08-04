package resolver

import (
	"errors"

	"github.com/masp/ertylang/ast"
	"github.com/masp/ertylang/types"
)

var (
	// ErrNoImporter is only used for base modules that should never import other modules
	ErrNoImporter = errors.New("forbidden")

	// ErrModuleMissing is given when a module cannot be found at the path
	ErrModuleMissing = errors.New("module not found")
)

func (r *resolver) resolveImport(decl *ast.ImportDecl) {
	moduleName := decl.ModuleName()
	alias := decl.Alias
	if alias == nil {
		alias = &ast.Identifier{Name: moduleName}
	}
	imported, err := r.importModule(decl.Path.Value)
	if err != nil {
		r.error(decl, "could not import module '%s': %v", decl.Path.Value, err)
	}
	r.declare(alias, decl, &types.Module{
		AtomValue: types.AtomValue{V: moduleName},
		Imported:  imported,
	})
}

// Importer is any loader that given a module path can return a compiled and resolved
// module. The most basic implementation is a preloaded set of modules that can be loaded from memory.
// A more complicated implementation searches and parses files on-the-fly from disk.
type Importer interface {
	// Load searches for a module at path and returns it if possible. If the module is not
	// found, ErrModuleMissing is returned. If the module is found but malformed, the errors from
	// parsing are returned.
	Load(path string) (*ast.Module, error)
}

func (r *resolver) importModule(path string) (*ast.Module, error) {
	if r.importer == nil {
		return nil, ErrNoImporter
	}
	return r.importer.Load(path)
}

// CachedImporter returns a list of pre-loaded modules, which is useful for modules that are
// very commonly imported like OTP and ERTS modules.
type CachedImporter struct {
	LoadedModules map[string]*ast.Module // preloaded modules (key is module path)
}

// Load searches for a module with name path and returns it if possible. If the module is not
// found, ErrModuleMissing is returned. If the module is found but malformed, the errors from
// parsing are returned.
func (i *CachedImporter) Load(path string) (*ast.Module, error) {
	found, ok := i.LoadedModules[path]
	if !ok {
		return nil, ErrModuleMissing
	}
	return found, nil
}

// Add causes all future Load's of path to return module.
func (i *CachedImporter) Add(path string, module *ast.Module) {
	i.LoadedModules[path] = module
}
