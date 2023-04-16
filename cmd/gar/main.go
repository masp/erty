package main

import (
	"fmt"
	"os"

	"github.com/masp/garlang/cmd/gar/build"
	"github.com/masp/garlang/cmd/gar/run"
)

const VERSION = "0.1.0"

func printUsage() {
	fmt.Fprintf(os.Stdout, `Usage: gar <command> [command options]
	
Commands:
  build  Compile a gar module to Core Erlang or BEAM
  run    Run a garlang module directly in the shell

Run 'gar help <command>' for more information on a command.
`)
}

// main parses the first command line argument and dispatches the appropriate subcommand with
// the rest of the args.
func main() {
	if len(os.Args) < 2 {
		printUsage()
		os.Exit(1)
	}
	subargs := os.Args[2:]

	var err error
	switch os.Args[1] {
	case "build":
		err = build.Main(subargs)
	case "run":
		err = run.Main(subargs)
	case "help":
		if len(subargs) == 0 {
			printUsage()
			os.Exit(1)
		}
		subcmd := subargs[0]
		switch subcmd {
		case "build":
			fmt.Fprint(os.Stdout, build.Help)
		case "run":
			fmt.Fprint(os.Stdout, run.Help)
		default:
			fmt.Fprintf(os.Stderr, "error: unknown command %q\n", subcmd)
			os.Exit(1)
		}
	case "version":
		fmt.Fprintf(os.Stdout, "gar version %s\n", VERSION)
	default:
		fmt.Fprintf(os.Stderr, "error: unknown command %q\n", os.Args[1])
		os.Exit(1)
	}

	if err != nil {
		fmt.Fprintf(os.Stderr, "error: %v\n", err)
		os.Exit(1)
	}
}
