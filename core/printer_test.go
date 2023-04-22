package core

import (
	"bytes"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"github.com/sebdah/goldie/v2"
)

func TestPrintModule(t *testing.T) {
	tests := []struct {
		name     string
		input    *Module
		expected string
	}{
		{
			name: "one_func_annotated",
			input: &Module{
				Name: "one_func_annotated",
				Functions: []Func{
					{
						Name: FuncName{Name: "a", Arity: 0},
						Body: Atom{Value: "a"},
						Annotation: Annotation{
							Attrs: []Const{ConstTuple{Elements: []Const{
								Atom{Value: "function"},
								ConstTuple{Elements: []Const{
									Atom{Value: "a"}, Integer{Value: 0},
								}},
							}}},
						},
					},
				},
			},
			expected: "one_func_annotated.core",
		},
		{
			name: "exports",
			input: &Module{
				Name: "exports",
				Exports: []FuncName{
					{Name: "a", Arity: 0},
					{Name: "b", Arity: 1},
				},
				Functions: []Func{
					{
						Name: FuncName{Name: "a", Arity: 0},
						Body: Atom{Value: "a"},
					},
					{
						Name: FuncName{Name: "b", Arity: 1},
						Body: Atom{Value: "b"},
					},
				},
			},
			expected: "exports.core",
		},
		{
			name: "attributes",
			input: &Module{
				Name: "attributes",
				Attributes: []Attribute{
					{Key: Atom{Value: "a"}, Value: Atom{Value: "b"}},
					{Key: Atom{Value: "c"}, Value: ConstList{Elements: []Const{
						ConstTuple{Elements: []Const{Atom{Value: "d"}, Atom{Value: "e"}}},
						Atom{Value: "f"},
						Atom{Value: "g"},
					}}},
				},
				Functions: []Func{
					{
						Name: FuncName{Name: "a", Arity: 0},
						Body: Atom{Value: "a"},
					},
				},
			},
			expected: "attributes.core",
		},
		{
			name: "intermodule",
			input: &Module{
				Name: "intermodule",
				Functions: []Func{
					{
						Name: FuncName{Name: "a", Arity: 0},
						Body: InterModuleCall{
							Module: Application{
								Func: Atom{Value: "app"},
								Args: []Expr{Atom{Value: "c"}},
							},
							Func: Atom{Value: "c"},
							Args: []Expr{Atom{Value: "d"}},
						},
					},
				},
			},
			expected: "intermodule.core",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var out bytes.Buffer
			NewPrinter(&out).PrintModule(tt.input)
			g := goldie.New(t)
			g.Assert(t, tt.expected, out.Bytes())
		})
	}
}

func TestErlcCompiles(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping test in short mode")
	}

	erlcPath, err := exec.LookPath("erlc")
	if err != nil {
		t.Skip("erlc not found")
	}

	tmp := t.TempDir()
	tests := []string{"attributes.core", "exports.core", "one_func_annotated.core", "intermodule.core"}
	for _, test := range tests {
		t.Run(test, func(t *testing.T) {
			copyFile(t, filepath.Join("testdata", test+".golden"), filepath.Join(tmp, test))
			erlc := exec.Command(erlcPath, "+from_core", "-o", t.TempDir(), filepath.Join(tmp, test))
			output, err := erlc.CombinedOutput()
			if err != nil {
				t.Logf("Output: %s", string(output))
				t.Fatalf("erlc: %v", err)
			}
		})
	}
}

func copyFile(t *testing.T, src, dst string) {
	t.Helper()
	t.Logf("copying %s to %s", src, dst)

	srcFile, err := os.Open(src)
	if err != nil {
		t.Fatalf("open source file: %v", err)
	}
	defer srcFile.Close()

	dstFile, err := os.Create(dst)
	if err != nil {
		t.Fatalf("create destination file: %v", err)
	}
	defer dstFile.Close()

	if _, err := io.Copy(dstFile, srcFile); err != nil {
		t.Fatalf("copy file: %v", err)
	}
}
