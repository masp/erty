package parse

import (
	"testing"
)

func FuzzParse(f *testing.F) {
	f.Add(`module test; func main() { test = "hello world" }`)
	f.Add("module abcd")
	f.Add("module A func A")
	f.Add("module A(func A")

	f.Fuzz(func(t *testing.T, input string) {
		mod, _ := Module("<test>", []byte(input))
		if mod == nil {
			t.Fatalf("Expect non-nil module always")
		}
	})
}
