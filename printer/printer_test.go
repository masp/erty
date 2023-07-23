package printer

import (
	"strings"
	"testing"

	"github.com/masp/ertylang/ast"
	"github.com/masp/ertylang/parser"
	"github.com/masp/ertylang/token"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func parse(program string) (*token.File, ast.Expression) {
	file, expr, err := parser.ParseExpr(program)
	if err != nil {
		panic(err)
	}
	return file, expr
}

func TestFormatExpr(t *testing.T) {
	tests := []struct {
		in, want string
	}{
		{"1+1*2", "1 + 1 * 2"},
		{"fun(a,1+2)", "fun(a, 1 + 2)"},
		{"a. fun()", "a.fun()"},
		{"a    int", "a int"},
	}

	for _, test := range tests {
		t.Run(test.in, func(t *testing.T) {
			cfg := &Config{}
			var got strings.Builder
			file, expr := parse(test.in)
			err := cfg.Fprint(&got, file, expr)
			require.NoError(t, err)
			assert.Equal(t, test.want, got.String())
		})
	}
}
