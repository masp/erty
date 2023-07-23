package types

import (
	"testing"

	"github.com/masp/ertylang/ast"
	"github.com/stretchr/testify/assert"
)

func TestUnify(t *testing.T) {
	tests := map[string]struct {
		value, pattern ast.Type
		want           ast.Type
	}{
		"matching integer types":   {Int, Int, Int},
		"untyped int with integer": {UntypedInt, Int, Int},
		"enum":                     {Bool, Bool, Bool},
		"subset of enum":           {Bool, &AtomValue{V: "true"}, &AtomValue{V: "true"}},
		"bad":                      {Int, String, Invalid},
	}

	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			got := Unify(tt.pattern, tt.value)
			assert.Equal(t, tt.want, got)
		})
	}
}

func TestMerge(t *testing.T) {
	tests := map[string]struct {
		ts   []ast.Type
		want ast.Type
	}{
		"ints":  {[]ast.Type{Int, UntypedInt, Int, Int}, Int},
		"mixed": {[]ast.Type{Int, String, Int}, &Enum{Cases: []ast.Type{Int, String}}},
		"enums": {
			ts: []ast.Type{
				&Enum{Cases: []ast.Type{Int, String}},
				Float, Int,
			},
			want: &Enum{Cases: []ast.Type{Int, String, Float}},
		},
	}

	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			got := Merge(tt.ts...)
			assert.Equal(t, tt.want, got)
		})
	}
}
