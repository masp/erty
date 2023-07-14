package types

import "github.com/masp/garlang/ast"

var Builtins = map[string]ast.Type{
	"int":    Int,
	"float":  Float,
	"string": String,
	"atom":   Atom,
	"any":    Any,
}

func init() {
	for n, t := range Builtins {
		Builtins[n] = &Expr{t}
	}
}
