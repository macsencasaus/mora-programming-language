package main

import (
	"fmt"
	"os"
	"os/user"

	"mora/interpreter"
	"mora/repl"
)

func main() {
	user, err := user.Current()
	if err != nil {
		panic(err)
	}

	if len(os.Args) < 2 {
		fmt.Printf("Hello %s! This is the mora programming language!\n",
			user.Username)
		fmt.Printf("Feel free to type in commands\n")
		repl.Start(os.Stdin, os.Stdout)
	} else {
		interpreter.Start(os.Args[1], os.Stdout)
	}
}
