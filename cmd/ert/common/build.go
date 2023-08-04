package common

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/masp/ertylang/compiler"
	"github.com/masp/ertylang/core"
	"github.com/masp/ertylang/parser"
	"github.com/masp/ertylang/resolver"
	"github.com/masp/ertylang/token"
)

func BuildTmp(input string) (string, error) {
	inputName := filepath.Base(input)
	inputSrc, err := os.ReadFile(input)
	if err != nil {
		return "", fmt.Errorf("reading input '%s': %w", input, err)
	}

	ertModule, err := parser.ParseModule(inputName, inputSrc, nil)
	if err != nil {
		return "", PrintErr(inputName, "parse", err)
	}

	err = resolver.ResolveModule(ertModule, nil)
	if err != nil {
		return "", PrintErr(inputName, "compile", err)
	}

	cmplr := compiler.New()
	mod, err := cmplr.CompileModule(ertModule)
	if err != nil {
		return "", PrintErr(inputName, "compile", err)
	}

	outputFile, err := os.CreateTemp("", fmt.Sprintf("%s.*.core", mod.Name))
	if err != nil {
		return "", fmt.Errorf("write core output to: %w", err)
	}
	defer outputFile.Close()
	core.NewPrinter(outputFile).PrintModule(mod)
	return outputFile.Name(), nil
}

// PrintErr prints a token error list to stderr and returns the summary error
func PrintErr(filename string, stage string, err error) error {
	if lexErrs, ok := err.(token.ErrorList); ok {
		for _, err := range lexErrs {
			fmt.Fprintf(os.Stderr, "%s:%d:%d: %v\n", filename, err.Pos.Line, err.Pos.Column, err.Msg)
		}
		return fmt.Errorf("compile failed (%d errors)", lexErrs.Len())
	} else if err != nil {
		return fmt.Errorf("%s: %w", stage, err)
	}
	return nil
}

func TrimExt(path string) string {
	return strings.TrimSuffix(path, filepath.Ext(path))
}
