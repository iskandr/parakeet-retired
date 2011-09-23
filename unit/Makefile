all: front_end_ast

front_end_ast:
	gcc -o test_front_end_ast front_end_ast_test.c -I../FrontEnd \
	  -L../_build -lparakeetpy

clean:
	rm -f *~ *.o test_front_end_ast

