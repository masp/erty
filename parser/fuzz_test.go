package parser

import (
	"testing"
)

func FuzzParse(f *testing.F) {
	f.Add(`module test; func main() { test = "hello world" }`)
	f.Add("module abcd")
	f.Add("module A func A")
	f.Add("module A(func A()10;")
	f.Add("module A(func A()1\"\".")
	f.Add("module A; func A()\n1()=")

	f.Fuzz(func(t *testing.T, input string) {
		mod, _ := Module("<test>", []byte(input))
		if mod == nil {
			t.Fatalf("Expect non-nil module always")
		}
	})
}
