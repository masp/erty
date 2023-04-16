package run

// TODO

// Help is the help text for the run subcommand
const Help = `Usage: gar run [options] <file>

Run builds and runs a gar module directly in an ERTS shell. Multiple modules
can be specified with either a directory or glob pattern like './...' which compiles
and loads every module in the current directory and subdirectories.

TODO: Describe how to load a module that should be run in a supervision tree.

Options:
`

func Main(args []string) error {
	return nil
}
