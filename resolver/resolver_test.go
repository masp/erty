package resolver

import (
	"fmt"
	"testing"

	"github.com/masp/ertylang/ast"
	"github.com/masp/ertylang/parser"
	"github.com/masp/ertylang/token"
	"github.com/masp/ertylang/types"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestResolution(t *testing.T) {
	// Each test is a source that will be compiled to an AST and resolution run over the AST. Identifier pointers are encoded
	// as string like `name@line:col`. If a name is resolved, it points to the declaration. If it is unresolved, it is the wantUnresolved
	// list.
	tests := []struct {
		name string
		src  string
		want []string
	}{
		{
			name: "simple",
			src: `module test; func abc() {
x := 1
x + y
}

func other() {
	abc()
}`,
			want: []string{
				"test@1:8 -> mod[test@1:8]",
				"abc@1:19 -> func[abc@1:19]",
				"x@2:1 -> let[x@2:1]",
				"x@3:1 -> let[x@2:1]",
				"other@6:6 -> func[other@6:6]",
				"abc@7:2 -> func[abc@1:19]",
				"y@3:5 -> unresolved",
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			mod, err := parser.ParseModule("<test>", []byte(tt.src))
			require.NoError(t, err)

			err = ResolveModule(mod, nil)
			if len(tt.want) == 0 {
				require.NoError(t, err)
			} else {
				require.Error(t, err)
			}
			ids := collectIds(mod)

			var got []string
			for _, id := range ids {
				got = append(got, encodeRefersTo(mod.File, id))
			}

			assert.ElementsMatch(t, tt.want, got)
		})
	}
}

func TestTypeResolve(t *testing.T) {
	tests := []struct {
		name         string
		src          string
		wantResolved map[string]ast.Type
	}{
		{
			name: "all binary numeric ops",
			src: `module test; func abc(arg1, arg2 int, arg3 atom) string {
y := 1 * 3
x := 1 * 2 / 10.5 - 100 + 1.5
z := "a" + "b"
a := 1 == 2
b := !'true'
c := -3
d := int(c) + 10
e := ["a", z, "c"]
}`,
			wantResolved: map[string]ast.Type{
				"abc@1:19": &types.Func{
					Args: []ast.Type{
						types.Builtins["int"],
						types.Builtins["int"],
						types.Builtins["atom"],
					},
					Return: types.Builtins["string"],
				},
				"test@1:8":  &types.Module{AtomValue: types.AtomValue{V: "test"}},
				"arg1@1:23": types.Int,
				"arg2@1:29": types.Int,
				"int@1:34":  types.Int,
				"arg3@1:39": types.Atom,
				"atom@1:44": types.Atom,
				"y@2:1":     types.Int,
				"x@3:1":     types.Float,
				"z@4:1":     types.String,
				"a@5:1":     types.Bool,
				"b@6:1":     types.Bool,
				"c@7:1":     types.Int,
				"d@8:1":     types.Int,
				"int@8:6":   types.Int,
				"c@8:10":    types.Int,
				"e@9:1":     &types.List{Elem: types.String},
				"z@9:12":    types.String,
			},
		},
		{
			name: "import modules",
			src: `module test; import "erlang"

func add(a, b int) int {
	return a + b
}

func _(c int) {
	v := erlang.spawn(add(10, c), c)
}`,
			wantResolved: map[string]ast.Type{
				"test@1:8":   &types.Module{AtomValue: types.AtomValue{V: "test"}},
				"add@3:6":    &types.Func{Args: []ast.Type{types.Builtins["int"], types.Builtins["int"]}, Return: types.Builtins["int"]},
				"a@3:10":     types.Int,
				"b@3:13":     types.Int,
				"int@3:15":   types.Int,
				"a@4:9":      types.Int,
				"b@4:13":     types.Int,
				"_@7:6":      &types.Func{Args: []ast.Type{types.Builtins["int"]}, Return: types.Void},
				"c@7:8":      types.Int,
				"int@7:10":   types.Int,
				"v@8:2":      types.Any,
				"add@8:20":   &types.Func{Args: []ast.Type{types.Builtins["int"], types.Builtins["int"]}, Return: types.Builtins["int"]},
				"c@8:28":     types.Int,
				"c@8:32":     types.Int,
				"erlang@8:7": &types.Module{AtomValue: types.AtomValue{V: "erlang"}},
				"spawn@8:14": types.Any,
			},
		}}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			mod, err := parser.ParseModule("<test>", []byte(tt.src))
			require.NoError(t, err)

			err = ResolveModule(mod, nil)
			require.NoError(t, err)

			ids := collectIds(mod)

			got := make(map[string]ast.Type)
			for _, id := range ids {
				assert.NotNil(t, id.Type(), "id %s is unresolved", id.Name)
				enc := encodeIdent(mod.File, id)
				got[enc] = types.Value(id.Type())
			}
			assert.Equal(t, tt.wantResolved, got)
		})
	}
}

func TestResolveExpression(t *testing.T) {
	premodule := `
	module test
	func add(a, b int) int {
		return a + b
	}
	
	func main() int { got := `
	postmodule := `}`

	tests := map[string]struct {
		eval string
		want ast.Type
	}{
		"int": {`10`, types.Int},
		"match statement": {
			eval: `match 10+12 {
				case a int: return a
				case 10: 10
			}`,
			want: types.Int,
		},
	}

	for _, tt := range tests {
		t.Run(tt.eval, func(t *testing.T) {
			src := premodule + tt.eval + postmodule
			mod, err := parser.ParseModule("<test>", []byte(src))
			require.NoError(t, err)

			err = ResolveModule(mod, nil)
			require.NoError(t, err)
			var gotId *ast.Identifier
			ids := collectIds(mod)
			for _, id := range ids {
				if id.Name == "got" {
					gotId = id
				}
			}
			require.NotNil(t, gotId, "testValue was not found in the program")

			assert.Equal(t, tt.want, types.Deref(gotId.Type()))
		})
	}
}

func TestTypeResolveErrorExpr(t *testing.T) {
	tests := []struct {
		src     string
		wantErr string
	}{
		{`!'not bool'`, `<string>:1:2: operator ! not defined on 'not bool' (atom 'not bool')`},
		{`"a"*3`, `<string>:1:1: operator * not defined on "a" (untyped string)`},
		{`3*'c'`, `<string>:1:3: operator * not defined on 'c' (atom 'c')`},
		{`-"b"`, `<string>:1:2: operator - not defined on "b" (untyped string)`},
		{`3+"b"`, `operator + has mismatched types: untyped int and untyped string`},
		{`[3, "a"]`, `cannot use "a" (untyped string) as value in untyped int list`},
		{`match 10 { case "hello": 10 }`, `cannot unify value 10 (untyped int) with case pattern "hello" (untyped string)`},
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

func TestTypeResolveErrorModule(t *testing.T) {
	premodule := `
module test
func add(a, b int) int {
	return a + b
}

func main() {`
	postmodule := `}`

	tests := []struct {
		src     string
		wantErr string
	}{
		{`add(1, 2, 3)`, `too many arguments to func add (expected 2, got 3)`},
		{`add("a", 1)`, `cannot use "a" (untyped string) as int value in argument to add`},
		{`"add"(1, 2)`, `cannot call non-function "add" (variable of type untyped string)`},
		{`a := int("abc")`, `cannot cast untyped string to int`},
		{`return 10`, `cannot return untyped int from function of return type void`},
	}

	for _, tt := range tests {
		t.Run(tt.src, func(t *testing.T) {
			src := premodule + tt.src + postmodule
			mod, err := parser.ParseModule("<test>", []byte(src))
			require.NoError(t, err)

			err = ResolveModule(mod, nil)
			assert.ErrorContains(t, err, tt.wantErr)
		})
	}
}

func collectIds(n ast.Node) []*ast.Identifier {
	var ids []*ast.Identifier
	ast.Walk(n, ast.VisitorFunc(func(n ast.Node) error {
		if id, ok := n.(*ast.Identifier); ok {
			ids = append(ids, id)
		}
		return nil
	}))
	return ids
}

func encodeRefersTo(file *token.File, id *ast.Identifier) (result string) {
	if id.Type() == nil {
		return fmt.Sprintf("%s -> unresolved", encodeIdent(file, id))
	}

	if decl, ok := id.Type().(*types.Decl); ok {
		return encodeDecl(file, id, decl.RefersTo)
	}
	return encodeIdent(file, id)
}

func encodeDecl(file *token.File, id *ast.Identifier, refers ast.Node) (result string) {
	var keyword string
	var srcId *ast.Identifier
	switch d := refers.(type) {
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
		panic(fmt.Errorf("unrecognized node type for refers to decl: %T", refers))
	}
	return fmt.Sprintf("%s -> %s[%s]", encodeIdent(file, id), keyword, encodeIdent(file, srcId))
}

func encodeIdent(file *token.File, id *ast.Identifier) string {
	position := file.Position(id.Pos())
	return fmt.Sprintf("%s@%d:%d", id.Name, position.Line, position.Column)
}
