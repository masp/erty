package main

import (
	"fmt"
	"os"

	"github.com/masp/garlang/lexer"
	"github.com/masp/garlang/parse"
)

func main() {
	inputFile, err := os.ReadFile(os.Args[1])
	if err != nil {
		panic(err)
	}

	lex := lexer.NewLexer(string(inputFile))

	var tokens []lexer.Token
	for {
		token := lex.NextToken()
		if token.Type == lexer.EOF {
			if lex.HasErrors() {
				for _, err := range lex.Errors() {
					fmt.Println(err)
				}
			}
			break
		}
		tokens = append(tokens, token)
	}

	program, err := parse.Module(tokens)
	if err != nil {
		panic(err)
	}

	parse.NewPrinter(os.Stdout).Print(program)
}
