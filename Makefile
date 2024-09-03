test:
	go test -v ./lexer/ 
	go test -v ./parser/

build:
	go build

run:
	rlwrap go run .
