fuzz:
	go test github.com/masp/garlang/lexer -fuzz FuzzLex

install:
	go install ./...