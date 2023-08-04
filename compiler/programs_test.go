package compiler

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"github.com/masp/ertylang/core"
	"github.com/masp/ertylang/parser"
	"github.com/masp/ertylang/resolver"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// These tests compile and run programs and compare the output to validate correctness.
// They test a large scope of features and are only meant to be used for regression testing.

func TestPrograms(t *testing.T) {
	// Iterate through all test files under the testdata/programs/ folder
	// and run them through the compiler.
	if testing.Short() {
		t.Skip("skipping full program tests in short mode")
	}
	err := filepath.Walk("testdata/programs/", func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		if !info.IsDir() && filepath.Ext(path) == ".test" {
			t.Run(filepath.Base(path), func(t *testing.T) {
				runProgramTest(t, path)
			})
		}
		return nil
	})

	if err != nil {
		fmt.Println("Error:", err)
	}

}

func runProgramTest(t *testing.T, path string) {
	// Strip extension
	testName := strings.TrimSuffix(path, filepath.Ext(path))

	testFile, err := parseTestFile(path)
	if err != nil {
		t.Fatalf("parse test file: %v", err)
	}
	t.Logf("Running test: %s (input=%s, output=%s)", path, testFile.Input, testFile.Output)

	// Compile the program
	cmplr := New()
	module, err := parser.ParseModule(testName+".ert", []byte(testFile.ProgramBody), nil)
	if err != nil {
		t.Fatalf("parse module: %v", err)
	}

	err = resolver.ResolveModule(module, &resolver.Config{Importer: resolver.BuiltinsImporter})
	if err != nil {
		t.Fatalf("resolve module: %v", err)
	}

	compiled, err := cmplr.CompileModule(module)
	if err != nil {
		t.Fatalf("compile module: %v", err)
	}

	outDir := t.TempDir()
	corePath := filepath.Join(outDir, module.Id.Name+".core")
	coreFile, err := os.OpenFile(corePath, os.O_CREATE|os.O_WRONLY, 0644)
	require.NoError(t, err)
	core.NewPrinter(coreFile).PrintModule(compiled)

	t.Logf("Compiled module to %s", corePath)

	// Compile the program to beam and execute it
	output, err := compileAndExecute(t, module.Id.Name, coreFile.Name(), strings.Split(testFile.Input, " "))
	if err != nil {
		// move coreFile to the testdata/programs/ folder
		debugCorePath := filepath.Join("testdata/programs/", filepath.Base(coreFile.Name()))
		err = os.Rename(coreFile.Name(), debugCorePath)
		t.Logf("Writing compiled core file to %s to be examined", debugCorePath)
		t.Fatalf("compile and execute: %v", err)
	}
	assert.Equal(t, testFile.Output, output)
}

type TestFile struct {
	Input       string
	Output      string
	ProgramBody string
}

// parseTestFile conerts a file in the following form:
// input=1 2 3
// output=output here
// ----
// module ops
// func main() { ... }

func parseTestFile(path string) (*TestFile, error) {
	rawContents, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}

	contents := string(rawContents)

	programParts := strings.Split(contents, "---")
	if len(programParts) != 2 {
		return nil, fmt.Errorf("missing or too many --- dividers: %d sections", len(programParts))
	}

	// Key value pairs per line:
	// input={input}
	// output={output}

	attrs := make(map[string]string)
	for _, line := range strings.Split(programParts[0], "\n") {
		line = strings.TrimSpace(line)
		if len(line) == 0 {
			continue
		}
		ps := strings.SplitN(line, "=", 2)
		if len(ps) != 2 {
			return nil, fmt.Errorf("bad desc line (expected key=value): %s", line)
		}
		attrs[strings.TrimSpace(ps[0])] = strings.TrimSpace(ps[1])
	}

	if attrs["output"] == "" {
		return nil, fmt.Errorf("missing output attribute")
	}
	// Program body is the whole second part after
	programBody := strings.Trim(programParts[1], "-\n")
	return &TestFile{
		Input:       attrs["input"],
		Output:      attrs["output"],
		ProgramBody: programBody,
	}, nil
}

func compileAndExecute(t *testing.T, moduleName, coreFilePath string, inputArgs []string) (string, error) {
	beamPath := filepath.Join(filepath.Dir(coreFilePath), "beam")
	err := os.Mkdir(beamPath, 0755)
	if err != nil {
		return "", err
	}
	// Compile the Core file to beam/
	t.Logf("Compiling %s to %s", coreFilePath, beamPath)
	cmdCompile := exec.Command("erlc", "-o", beamPath, "+from_core", "+to_beam", coreFilePath)
	if stderr, err := cmdCompile.CombinedOutput(); err != nil {
		t.Logf("erlc: %s", string(stderr))
		return "", fmt.Errorf("compile core files: %w", err)
	}

	// Execute the Beam file
	args := []string{"-s", moduleName, "main"}
	args = append(args, inputArgs...)
	args = append(args, "-s", "erlang", "halt", "-noshell", "-noinput", "-pa", beamPath)
	t.Logf("erl %s", strings.Join(args, " "))
	cmdExecute := exec.Command("erl", args...)
	var stdout, stderr bytes.Buffer
	cmdExecute.Stdout = &stdout
	cmdExecute.Stderr = &stderr
	if err := cmdExecute.Run(); err != nil {
		t.Logf("erl: %s", stderr.String())
		return "", fmt.Errorf("execute beam main file: %w", err)
	}
	return stdout.String(), nil
}
