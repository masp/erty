package run

import (
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/masp/ertylang/cmd/ert/common"
)

// TODO

// Help is the help text for the run subcommand
const Help = `Usage: ert run [options] <file> [args...]

Run builds and runs a erty module directly in an ERTS shell. Multiple modules
can be specified with either a directory or glob pattern like './...' which compiles
and loads every module in the current directory and subdirectories.

TODO: Describe how to load a module that should be run in a supervision tree.

Options:
	-v      Print verbose output from the compiler
	-shell  Open an Erlang shell with the compiled module loaded
`

var (
	flagVerbose bool
	flagShell   bool
)

func parseFlags(args []string) (*flag.FlagSet, error) {
	fset := flag.NewFlagSet("run", flag.ContinueOnError)
	fset.BoolVar(&flagVerbose, "v", false, "")
	fset.BoolVar(&flagShell, "shell", false, "")
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
	output, err := common.BuildTmp(input)
	if err != nil {
		return err
	}
	return startShell(common.TrimExt(filepath.Base(input)), output, flags.Args()[1:]...)
}

func startShell(modName, coreFile string, cmdArgs ...string) error {
	buildDir, err := os.MkdirTemp("", "ertc-*")
	if err != nil {
		return err
	}

	oldCoreFile := coreFile
	coreFile = filepath.Join(buildDir, modName+".core")
	err = os.Rename(oldCoreFile, coreFile)
	if err != nil {
		return err
	}

	beamPath := buildDir
	cmdCompile := exec.Command("erlc", "-o", beamPath, "+from_core", "+to_beam", coreFile)
	cmdCompile.Stdout = os.Stdout
	cmdCompile.Stderr = os.Stderr
	if err := cmdCompile.Run(); err != nil {
		return fmt.Errorf("erlc: %w", err)
	}

	// Execute the Beam file
	var args []string
	if !flagShell {
		args = append(args, "-run", modName, "main")
		// always pass name of module as first arg
		args = append(args, modName)
		args = append(args, cmdArgs...)
		args = append(args, "-noshell", "-s", "erlang", "halt")
	}
	args = append(args, "-pa", beamPath)
	if flagVerbose {
		fmt.Printf("exec: erl %s\n", strings.Join(args, " "))
	}
	cmdExecute := exec.Command("erl", args...)
	cmdExecute.Stdout = os.Stdout
	cmdExecute.Stderr = os.Stderr
	cmdExecute.Stdin = os.Stdin
	return cmdExecute.Run()
}
