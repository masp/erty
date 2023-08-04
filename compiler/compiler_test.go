package compiler

import (
	"bytes"
	"testing"

	"github.com/masp/ertylang/core"
	"github.com/masp/ertylang/parser"
	"github.com/masp/ertylang/resolver"
	"github.com/sebdah/goldie/v2"
	"github.com/stretchr/testify/require"
)

func TestCompileModule(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{
			input:    `module mod; func a() int { return 1 }`,
			expected: "mod.core",
		},
	}

	for _, tt := range tests {
		t.Run(tt.input, func(t *testing.T) {
			mod, err := parser.ParseModule("<test>", []byte(tt.input), nil)
			if err != nil {
				t.Fatalf("parse program: %v", err)
			}

			err = resolver.ResolveModule(mod, nil)
			require.NoError(t, err)

			compiled, err := New().CompileModule(mod)
			require.NoError(t, err)

			var out bytes.Buffer
			core.NewPrinter(&out).PrintModule(compiled)
			g := goldie.New(t)
			g.Assert(t, tt.expected, out.Bytes())
		})
	}

}

func TestCompileFunc(t *testing.T) {
	premodule := `module testcompile

import "erlang"

func add(a, b int) int { return a + b }
`
	tests := []struct {
		input    string
		expected string
	}{
		{
			input: `
		func test(v, d int) int {
			a := 'erlang'.a(v)
			'erlang'.d()
			return int('erlang'.b(d)) + 100
		}`,
			expected: "arithm.core",
		},
		{
			input:    `func test() any {return 'a'}`,
			expected: "a.core",
		},
		{
			input:    `func test() any { return erlang.module_info(module('b')) }`,
			expected: "call.core",
		},
		{
			input: `func test(n int) int {
				return match n {
					case 0: 0
					case 1: 1
					case n int: test(n-1) + test(n-2)
				}
			}`,
			expected: "fib.core",
		},
	}

	for _, test := range tests {
		t.Run(test.input, func(t *testing.T) {
			mod, err := parser.ParseModule("<test>", []byte(premodule+test.input), nil)
			if err != nil {
				t.Fatalf("parse program: %v", err)
			}

			err = resolver.ResolveModule(mod, &resolver.Config{Importer: resolver.BuiltinsImporter})
			require.NoError(t, err)

			compiled, err := New().CompileModule(mod)
			require.NoError(t, err)

			testFn := findFunc(compiled.Functions, "test")
			require.NotNil(t, testFn, "did not find func with name 'test'")

			var out bytes.Buffer
			core.NewPrinter(&out).PrintFunc(testFn)
			g := goldie.New(t)
			g.Assert(t, test.expected, out.Bytes())
		})
	}
}

func findFunc(funcs []*core.Func, name string) *core.Func {
	for _, f := range funcs {
		if f.Name.Name == name {
			return f
		}
	}
	return nil
}
