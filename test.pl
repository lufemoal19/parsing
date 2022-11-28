:- use_module(library(dcg/basics)).

xquery([L | R]) --> ws, let(L), ws, xquery(R), {!}.
xquery([]) --> eos.

let(let(X, E)) --> ws, "let", ws, id(X), ws, "=", ws, expr(E), ws.

id(id(I)) --> identifier(I).
num(num(N)) -->  number(N).

expr(X) --> id(X);num(X).


%%%%%%%%%%%%%%%%%%%%%%%%%% Lexer Xquery Utils %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ws --> (" ";"\t";"\n";"\r"), ws.
ws -->  [].

identifier(ID, [C | R], O) :- is_alpha(C), !, complete_id([C], ID,R, O).
complete_id(CL, ID, [C | R], O) :- is_alphanum(C), !,
                                   complete_id([C | CL], ID, R, O).
complete_id(CL, ID, I, I) :- reverse(CL, RCL), 
                             atom_codes(ID, RCL).
                             
is_alpha(C)    :- code_type(C, alpha), !.
is_alpha(C)    :- atom_codes('$_', L), member(C, L),!.
id_digit(C)    :- code_type(C, digit).
is_alphanum(C) :- is_alpha(C);id_digit(C), !.


test(L) :- 
    File = 'test.txt',
    read_file_to_codes(File, Codes, []),
    writeln(Codes),
    phrase(xquery(L), Codes)
.