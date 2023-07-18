package ast

import (
	"fmt"
	"strings"
	"testing"

	"github.com/masp/ertylang/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestWalk(t *testing.T) {
	// Given an input AST, make sure that ast.Walk with a print func produces the whole syntax tree
	tree := &Module{
		Decls: []Decl{
			&ImportDecl{Path: &StringLiteral{Value: "fmt"}, Alias: &Identifier{Name: "f"}},
			&FuncDecl{
				Name: &Identifier{
					Name: "main",
				},
				Parameters: &FieldList{},
				Statements: []Statement{
					&ExprStatement{
						Expression: &Identifier{Name: "a"},
					},
				},
			},
		},
	}

	var output strings.Builder
	err := Walk(tree, VisitorFunc(func(n Node) error {
		fmt.Fprintf(&output, "%T\n", n)
		return nil
	}))
	require.NoError(t, err)
	assert.Equal(t, `*ast.Module
*ast.Identifier
*ast.ImportDecl
*ast.Identifier
*ast.StringLiteral
*ast.FuncDecl
*ast.Identifier
*ast.ExprStatement
*ast.Identifier
`, output.String())
}

func TestWalkError(t *testing.T) {
	// Given an input AST, make sure that ast.Walk with a print func produces the whole syntax tree
	tree := &Module{
		Decls: []Decl{
			&ImportDecl{Path: &StringLiteral{Value: "fmt"}, Alias: &Identifier{Name: "f"}},
			&FuncDecl{
				Name: &Identifier{
					Name: "main",
				},
				Parameters: &FieldList{},
				Statements: []Statement{
					&ExprStatement{
						Expression: &BinaryExpr{
							Left: &Identifier{
								Name: "a",
							},
							Op: token.Plus,
							Right: &Identifier{
								Name: "b",
							},
						},
					},
				},
			},
		},
	}

	err := Walk(tree, VisitorFunc(func(n Node) error {
		if _, ok := n.(*BinaryExpr); ok {
			return fmt.Errorf("error")
		}
		return nil
	}))
	assert.EqualError(t, err, "error")
}
