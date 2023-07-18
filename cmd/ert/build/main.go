package build

import (
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	"github.com/masp/ertylang/cmd/ert/common"
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
	tmpOutput, err := common.BuildTmp(input)
	if err != nil {
		return err
	}

	output, err := findOutput(input)
	if err != nil {
		return err
	}

	if *flagBeam {
		defer os.Remove(output)
		erlc := exec.Command("erlc", "+to_beam", "+from_core", "-o", filepath.Dir(output), output)
		erlc.Stdout = os.Stdout
		erlc.Stderr = os.Stderr
		if err = erlc.Run(); err != nil {
			return fmt.Errorf("run erlc: %w", err)
		}
	} else {
		err = os.Rename(tmpOutput, output)
		if err != nil {
			return fmt.Errorf("rename tmp core output: %w", err)
		}
	}
	return nil
}

func findOutput(input string) (string, error) {
	dir := filepath.Dir(input)
	// remove extension from filename
	inputFilename := filepath.Base(input)
	if *flagOutput != "" {
		if stat, err := os.Stat(*flagOutput); err == nil && stat.IsDir() {
			return filepath.Join(*flagOutput, common.TrimExt(inputFilename)+".core"), nil
		}

		ext := filepath.Ext(*flagOutput)
		if ext == ".core" { // is core file, use path as is
			return *flagOutput, nil
		} else { // extension is not core, error
			return "", fmt.Errorf("output file does not have .core extension")
		}
	}
	return filepath.Join(dir, common.TrimExt(inputFilename)+".core"), nil
}
