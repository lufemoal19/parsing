/*
@about UrQuery parser
@authors 
    natalia.ar969@gmail.com
    lufemoal19@gmail.com
@since 2022
*/

:- module(parsing, [prog_urquery/3]).
:- use_module(lexing).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%% URQUERY PARSER %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prog_urquery(sequence(L)) --> urquery_list(L), {!}.
prog_urquery(none) --> [].

urquery_list([L | R]) --> (letprog(L); urquery(L)), urquery_list(R), {!}.
urquery_list([]) --> [].

letprog(let(I, E, U)) --> let, id(I), equals, expr(E), in, urquery(U).
letprog(let(I, E)) --> let, id(I), equals, expr(E).

urquery(urquery(I)) --> tagquery(I).

tagquery(tagquery(T,I)) --> left_tag, tag(T), right_tag, left_bracket, forquery(I), right_bracket, close_tag, tag(T), right_tag. 

forquery(forquery(V, I, P)) --> for, qvar(V), in, exprquery(I), return, varquery(P). 

exprquery(exprquery(I, P)) --> (sourcequery(I), startxpath(P) ; sourcequery(I)).

sourcequery(I) --> docpath(I) ; qvar(I). 

docpath(docpath(E)) --> doc, left_round, expr(E), right_round.

varquery(I) --> vartag(I); varpath(I).

vartag(vartag(T, I)) --> left_tag, tag(T), right_tag, left_bracket, varpath(I), right_bracket, close_tag, tag(T), right_tag. 

varpath(varpath(I, P)) --> qvar(I), startxpath(P).
varpath(varpath(qvar(I))) -->  qvar(I).

startxpath(I) --> slash, xpath(I).

xpath(xpath(I, R)) --> xml_id(I), slash, xpath(R).
xpath(xpath(I)) --> xml_id(I). 

qvar(qvar(I)) --> dolar, id(I), ws.
tag(tag(I)) --> ws, xml_id(I), ws.

expr(X) --> id(X); num(X); uq_string(X).

id(id(I)) --> identifier(I).
xml_id(I) --> identifier(I).
num(num(N)) --> number(N).
uq_string(expr(I)) --> quote, identifier(I), quote.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%% SYMBOL TABLE %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

quote --> ws, [39], ws.
slash --> [47].
left_tag --> ws, [60], ws.
right_tag --> ws, [62], ws.
close_tag --> ws, [60, 47], ws.
left_bracket --> ws, [123], ws.
right_bracket --> ws, [125], ws.
left_round --> ws, [40], ws.
right_round --> ws, [41], ws. 
dolar --> ws, [36].
equals --> ws, [61], ws.

let --> ws, [108, 101, 116], ws.
for --> ws, [102, 111, 114], ws.
doc --> ws, [100, 111, 99], ws.
in --> ws, [105, 110], ws.
return --> ws, [114, 101, 116, 117, 114, 110], ws.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%% UNIT TEST %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(L) :- 
    File = 'test.txt',
    read_file_to_codes(File, Codes, []),
    atom_codes(Input, Codes),
    format('Input=~n~s~n', [Input]),
    phrase(prog_urquery(L), Codes)
.