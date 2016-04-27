all: test

parser.ml: parser.mly
	ocamlyacc parser.mly
	rm parser.mli

lexer.ml: lexer.mll
	ocamllex lexer.mll

region: region.ml parser.ml lexer.ml main.ml
	ocamlc region.ml parser.ml lexer.ml main.ml -o region

test: region
	./region < test1.txt > result.txt ; diff result1.txt result.txt
	./region < test2.txt > result.txt ; diff result2.txt result.txt

clean:
	rm -rf *.cm* parser.ml parser.mli lexer.ml region result.txt
