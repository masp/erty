package types

import "github.com/masp/ertylang/ast"

var Builtins = map[string]ast.Type{
	"int":    Int,
	"char":   Int,
	"float":  Float,
	"string": String,
	"atom":   Atom,
	"any":    Any,
	"module": ModuleName,
}

func init() {
	for n, t := range Builtins {
		Builtins[n] = &Expr{t}
	}
}
