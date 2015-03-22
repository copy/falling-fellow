USE_APPOP=-pp "camlp4o ./../../operator-test/appop.cmo"

all: out.js

out.js: *.ml 
	# flags: g -> debug information
	#ocamlbuild -no-links -lflags '-g' -use-ocamlfind -pkgs js_of_ocaml,js_of_ocaml.syntax,containers -syntax camlp4o main.byte
	ocamlbuild -no-links -use-ocamlfind -pkgs js_of_ocaml,js_of_ocaml.syntax,containers -syntax camlp4o main.byte


	js_of_ocaml --opt 3 _build/main.byte -o out.js
	#js_of_ocaml --opt 1 --enable pretty --sourcemap js/lib.js _build/main.byte -o out.js


minify: out.js
	ls -lah out.js
	java -jar compiler.jar --js out.js --js_output_file out-min.js
	mv out-min.js out.js
	ls -lah out.js
