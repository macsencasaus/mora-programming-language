package interpreter

import (
	"io"
	"log"
	"os"

	"mora/ast"
	"mora/lexer"
	"mora/parser"
	"mora/semantics"
)

func Start(filepath string, out io.Writer) {
	content, err := os.ReadFile(filepath)
	if err != nil {
		log.Fatalf("Error reading file: %v\n", err)
	}
	environment := ast.NewEnvironment()

	l := lexer.New(string(content))
	p := parser.New(l, environment)

	program := p.ParseProgram()

	if len(p.Errors()) != 0 {
		printParserErrors(out, p.Errors())
		return
	}

	// io.WriteString(out, program.String())

	t := semantics.Check(program, nil, environment)

	io.WriteString(out, t.String()+"\n")
}

func printParserErrors(out io.Writer, errors []string) {
	for _, msg := range errors {
		io.WriteString(out, "\t"+msg+"\n")
	}
}
