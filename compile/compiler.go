// Package compiler emits Core Erlang from the AST generated by the parser.
package compile

import (
	"io"

	"github.com/masp/garlang/ast"
)

type Compiler struct {
	genPhase *gen
}

func New(out io.Writer) *Compiler {
	return &Compiler{genPhase: &gen{Output: out, indentSize: 4}}
}

func (c *Compiler) EmitModule(mod *ast.Module) {
	// addModuleInfo(mod)
	c.genPhase.EmitModule(mod)
}

// addModuleInfo adds the module_info functions to the module that Erlang requires as part of every
// module.
//
// The functions are very simple: just call 'erlang':module_info/1 with the appropriate atom.
// func addModuleInfo(mod *ast.Module) {
// 	mod.Functions = append(mod.Functions, ast.FuncDecl{
// 		Name: "module_info",
// 		Parameters: []ast.Parameter{
// 			{
// 				Name: "Attribute",
// 				Type: ast.TypeAtom,
// 			},
// 		},
// 		ReturnType: ast.AnyType,
// 		Body: []ast.Expr{
// 			&ast.CallExpr{
// 				Module:   "erlang",
// 				Function: "module_info",
// 			},
// 		},
// 	})
// 	mod.Functions = append(mod.Functions, &ast.FuncDecl{
// 		Name: "module_info",
// 		Parameters: []*ast.Parameter{
// 			{
// 				Name: "Attribute",
// 				Type: ast.AtomType,
// 			},
// 			{
// 				Name: "Value",
// 				Type: ast.AnyType,
// 			},
// 		},
// 		ReturnType: ast.AnyType,
// 		Body: []ast.Expr{
// 			&ast.CallExpr{
// 				Module:   "erlang",
// 				Function: "module_info",
// 				Arguments: []ast.Expr{
// 					&ast.AtomExpr{Value: "Attribute"},
// 					&ast.AtomExpr{Value: "Value"},
// 				},
// 			},
// 		},
// 	})
// }