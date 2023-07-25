fuzz:
	go test github.com/masp/ertylang/lexer -fuzz FuzzLex

install:
	go install ./...

test:
	go test ./...
