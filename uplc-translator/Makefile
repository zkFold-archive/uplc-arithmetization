run: translator
	./translator ./test/fib.uplc consts.json terms.json

translator: parser.cc lexer.cc lexer.h main.cc ast.cc eval.cc
	g++ --std=c++17 -g -o translator main.cc parser.cc lexer.cc ast.cc eval.cc

lexer.cc: lexer.l
	ragel -o lexer.cc lexer.l

parser.cc: parser.y
	bison -o parser.cc parser.y

clean:
	rm -f translator lexer.cc parser.cc parser.hh stack.hh
