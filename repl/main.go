package repl

import (
	"bufio"
	"fmt"
	"io"

	"mora/ast"
	"mora/evaluator"
	"mora/lexer"
	"mora/parser"
	"mora/semantics"
)

const PROMPT = ">> "

func Start(in io.Reader, out io.Writer) {
	scanner := bufio.NewScanner(in)
	environment := ast.NewEnvironment()

	for {
		fmt.Print(PROMPT)
		scanned := scanner.Scan()
		if !scanned {
			return
		}

		line := scanner.Text()
		l := lexer.New(line)
		p := parser.New(l, environment)

		program := p.ParseProgram()

		if len(p.Errors()) != 0 {
			printParserErrors(out, p.Errors())
			continue
		}

		t := semantics.Check(program, nil, environment)
		if semantics.IsError(t) {
			io.WriteString(out, t.String()+"\n")
			continue
		}

		// io.WriteString(out, program.String())

		// if t != nil {
		// 	io.WriteString(out, t.String()+"\n")
		// } else {
		// 	io.WriteString(out, "null\n")
		// }

		s := evaluator.Eval(program, environment)
		if s != nil {
			io.WriteString(out, s.Inspect()+"\n")
		}
	}
}

func printParserErrors(out io.Writer, errors []string) {
	for _, msg := range errors {
		io.WriteString(out, "\t"+msg+"\n")
	}
}
