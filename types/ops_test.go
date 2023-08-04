package types

import (
	"fmt"
	"testing"

	"github.com/masp/ertylang/ast"
	"github.com/stretchr/testify/assert"
)

func TestApplyOp(t *testing.T) {
	tests := map[string]struct {
		t1, t2  ast.Type
		want    ast.Type
		wantErr error
	}{
		"matching integer types":              {Int, Int, Int, nil},
		"untyped int with integer":            {UntypedInt, Int, Int, nil},
		"untyped int with integer reverse":    {Int, UntypedInt, Int, nil},
		"integer with float":                  {Int, Float, nil, ErrMismatch},
		"untyped float with float":            {UntypedFloat, Float, Float, nil},
		"untyped string with mismatched type": {UntypedString, Float, nil, ErrMismatch},
		"untyped string with matching string": {UntypedString, String, String, nil},
		"untyped and untyped":                 {UntypedInt, UntypedInt, UntypedInt, nil},
		"untyped int with untyped float":      {UntypedInt, UntypedFloat, UntypedFloat, nil},
	}

	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			got, err := ApplyOp(tt.t1, tt.t2)
			assert.Equal(t, tt.want, got)
			assert.Equal(t, tt.wantErr, err)
		})
	}
}

func TestIsAssignable(t *testing.T) {
	tests := []struct {
		to, value ast.Type
	}{
		{Int, Int},
		{Int, UntypedInt},
		{Any, String},
		{&Expr{Definition: Int}, Int},
		{&List{Elem: Any}, &List{Elem: Int}},
	}

	for _, tt := range tests {
		t.Run(fmt.Sprintf("%s = %s", tt.to, tt.value), func(t *testing.T) {
			got := IsAssignable(tt.to, tt.value)
			assert.True(t, got, "expected %s to be assignable to %s", tt.value, tt.to)
		})
	}
}

func TestIsNotAssignable(t *testing.T) {
	tests := []struct {
		to, value ast.Type
	}{
		{Int, String},
		{&List{Elem: Int}, &List{Elem: String}},
		{&List{Elem: Int}, Int},
	}

	for _, tt := range tests {
		t.Run(fmt.Sprintf("%s = %s", tt.to, tt.value), func(t *testing.T) {
			assignable := IsAssignable(tt.to, tt.value)
			assert.False(t, assignable, "expected %s to NOT be assignable to %s", tt.value, tt.to)
		})
	}
}
