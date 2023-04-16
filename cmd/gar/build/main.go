package build

import (
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/masp/garlang/compile"
	"github.com/masp/garlang/core"
	"github.com/masp/garlang/lexer"
	"github.com/masp/garlang/parse"
	"github.com/masp/garlang/token"
)

const Help = `Usage: gar build [options] <file>

Options:
  -o <file>  Write output to <file> instead. Default: <inputpath>.core
  -beam      Compile to BEAM instead of Core Erlang
`

var (
	flagOutput *string
	flagBeam   *bool
)

func parseFlags(args []string) (*flag.FlagSet, error) {
	fset := flag.NewFlagSet("build", flag.ContinueOnError)
	flagOutput = fset.String("o", "", "")
	flagBeam = fset.Bool("beam", false, "")
	fset.Usage = func() {
		fmt.Fprint(os.Stdout, Help)
	}
	err := fset.Parse(args)
	if err == flag.ErrHelp {
		fmt.Fprint(os.Stdout, Help)
		os.Exit(0)
	} else if err != nil {
		return nil, err
	}
	return fset, nil
}

func Main(args []string) error {
	flags, err := parseFlags(args)
	if err != nil {
		return err
	}

	input := flags.Arg(0)
	inputSrc, err := os.ReadFile(input)
	if err != nil {
		return fmt.Errorf("reading input '%s': %w", input, err)
	}

	lex := lexer.NewLexer(string(inputSrc))
	var tokens []lexer.Token
	for {
		tok := lex.NextToken()
		if tok.Type == token.EOF {
			if lex.HasErrors() {
				for _, err := range lex.Errors() {
					fmt.Println(err)
				}
			}
			break
		}
		tokens = append(tokens, tok)
	}

	garMod, err := parse.Module(tokens)
	if err != nil {
		return fmt.Errorf("parse: %w", err)
	}

	coreMod, err := compile.New().CompileModule(garMod)
	if err != nil {
		return fmt.Errorf("compile: %w", err)
	}

	output, err := findOutput(input)
	if err != nil {
		return err
	}
	outputFile, err := os.OpenFile(output, os.O_CREATE|os.O_WRONLY, 0644)
	if err != nil {
		return fmt.Errorf("write output '%s': %w", output, err)
	}
	core.NewPrinter(outputFile).PrintModule(coreMod)

	if *flagBeam {
		defer os.Remove(output)
		erlc := exec.Command("erlc", "+to_beam", "+from_core", "-o", filepath.Dir(output), output)
		erlc.Stdout = os.Stdout
		erlc.Stderr = os.Stderr
		if err = erlc.Run(); err != nil {
			return fmt.Errorf("run erlc: %w", err)
		}
	}
	return nil
}

func trimExt(path string) string {
	return strings.TrimSuffix(path, filepath.Ext(path))
}

func findOutput(input string) (string, error) {
	dir := filepath.Dir(input)
	// remove extension from filename
	inputFilename := filepath.Base(input)
	if *flagOutput != "" {
		if stat, err := os.Stat(*flagOutput); err == nil && stat.IsDir() {
			return filepath.Join(*flagOutput, trimExt(inputFilename)+".core"), nil
		}

		ext := filepath.Ext(*flagOutput)
		if ext == ".core" { // is core file, use path as is
			return *flagOutput, nil
		} else { // extension is not core, error
			return "", fmt.Errorf("output file does not have .core extension")
		}
	}
	return filepath.Join(dir, trimExt(inputFilename)+".core"), nil
}
