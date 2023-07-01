package resolver

import (
	"fmt"
	"testing"

	"github.com/masp/garlang/ast"
	"github.com/masp/garlang/parser"
	"github.com/masp/garlang/token"
	"github.com/masp/garlang/types"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestResolution(t *testing.T) {
	// Each test is a source that will be compiled to an AST and resolution run over the AST. Identifier pointers are encoded
	// as string like `name@line:col`. If a name is resolved, it points to the declaration. If it is unresolved, it is the wantUnresolved
	// list.
	tests := []struct {
		name           string
		src            string
		wantResolved   []string
		wantUnresolved []string
	}{
		{
			name: "simple",
			src: `module test; func _() {
x := 1
x + y
}`,
			wantResolved: []string{
				"test@1:8 -> mod[test@1:8]",
				"x@2:1 -> let[x@2:1]",
				"x@3:1 -> let[x@2:1]",
			},
			wantUnresolved: []string{
				"y@3:5",
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			mod, err := parser.ParseModule("<test>", []byte(tt.src))
			require.NoError(t, err)

			symbols, err := ResolveModule(mod, nil)
			if len(tt.wantUnresolved) == 0 {
				require.NoError(t, err)
			} else {
				require.Error(t, err)
			}
			assert.ElementsMatch(t, tt.wantResolved, encodeAll(mod.File, symbols.Resolved), "different resolved symbols")
			assert.ElementsMatch(t, tt.wantUnresolved, encodeUnresolved(mod.File, symbols.Unresolved), "missing or extra unresolved symbols")
		})
	}
}

func TestTypeResolve(t *testing.T) {
	tests := []struct {
		name         string
		src          string
		wantResolved map[string]types.Type
	}{{
		name: "all binary numeric ops",
		src: `module test; func _() {
y := 1 * 3
x := 1 * 2 / 10.5 - 100 + 1.5
z := "a" + "b"
a := 1 == 2
b := !'true'
c := -3
}`,
		wantResolved: map[string]types.Type{
			"y@2:1": types.Int,
			"x@3:1": types.Float,
			"z@4:1": types.String,
			"a@5:1": types.Bool,
		},
	}}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			mod, err := parser.ParseModule("<test>", []byte(tt.src))
			require.NoError(t, err)

			symbols, err := ResolveModule(mod, nil)
			require.NoError(t, err)

			for id, decl := range symbols.Resolved {
				wantType := tt.wantResolved[encodeIdent(mod.File, id)]
				if wantType == nil {
					continue
				}
				assert.Equal(t, wantType, decl.Type, "symbol type mismatched")
			}
		})
	}
}

func TestTypeResolveError(t *testing.T) {
	tests := []struct {
		src     string
		wantErr string
	}{
		{`!'not bool'`, `<string>:1:2: operator ! not defined on 'not bool' (atom 'not bool')`},
		{`"a"*3`, `<string>:1:1: operator * not defined on "a" (untyped string)`},
		{`3*'c'`, `<string>:1:3: operator * not defined on 'c' (atom 'c')`},
		{`-"b"`, `<string>:1:2: operator - not defined on "b" (untyped string)`},
		{`3+"b"`, `operator + has mismatched types: untyped int and untyped string`},
	}

	for _, tt := range tests {
		t.Run(tt.src, func(t *testing.T) {
			file, expr, err := parser.ParseExpr(tt.src)
			require.NoError(t, err)

			_, err = ResolveExpr(file, expr, nil)
			assert.ErrorContains(t, err, tt.wantErr)
		})
	}
}

func encodeIdent(file *token.File, id *ast.Identifier) string {
	position := file.Position(id.Pos())
	return fmt.Sprintf("%s@%d:%d", id.Name, position.Line, position.Column)
}

func encodeAll(file *token.File, resolved map[*ast.Identifier]types.Decl) (result []string) {
	for id, decl := range resolved {
		result = append(result, encode(file, id, decl))
	}
	return
}

func encode(file *token.File, id *ast.Identifier, decl types.Decl) (result string) {
	var keyword string
	var srcId *ast.Identifier
	switch d := decl.RefersTo.(type) {
	case *ast.Module:
		keyword = "mod"
		srcId = d.Id
	case *ast.MatchAssignExpr:
		keyword = "let"
		srcId = d.Left.(*ast.Identifier)
	case *ast.FuncDecl:
		keyword = "func"
		srcId = d.Name
	default:
		panic(fmt.Errorf("unrecognized node type for refers to decl: %T", decl.RefersTo))
	}
	return fmt.Sprintf("%s -> %s[%s]", encodeIdent(file, id), keyword, encodeIdent(file, srcId))
}

func encodeUnresolved(file *token.File, unresolved map[*ast.Identifier]struct{}) (result []string) {
	for id := range unresolved {
		result = append(result, encodeIdent(file, id))
	}
	return
}
