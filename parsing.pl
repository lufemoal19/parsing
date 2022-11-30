/*
@about UrQuery parser
@authors Grupo 3-1pm
@since 2022
*/

:- module(parsing, [prog_urquery/3]).
:- use_module(lexing).

main_urquery([L | R]) -->  ws, (let(L);prog_urquery(L);varquery(L);vartag(L);varpath(L)), ws, main_urquery(R), {!}.
main_urquery([]) --> [].

let(let(X, E)) --> ws, "let", ws, id(X), ws, "=", ws, expr(E), ws.

id(id(I)) --> identifier(I).
xml_id(I) --> identifier(I).
num(num(N)) -->  number(N).
uq_string(expr(I)) --> "'", identifier(I), "'".

expr(X) --> id(X);num(X);uq_string(X).

% qvar: $variable
qvar(qvar(I)) --> ws, [36], id(I), ws.
tag(tag(I)) --> ws, xml_id(I), ws.

xpath(xpath(I, R)) --> xml_id(I), "/", xpath(R).
xpath(xpath(I)) --> xml_id(I). 
startxpath(I) --> "/", xpath(I).

%$galleta/sabor 
% gramatica varpath varpath ->qvar (startxpath)?;
varpath(varpath(I, P)) --> (qvar(I), startxpath(P) ; qvar(I)).

% vartag -> "<" tag ">" "{" varpath "}" "</" tag ">";
% <h1>hola</h1>
% <a> $var/li </a>
vartag(vartag(T,I)) --> ws, "<", tag(T), ">", ws, "{", ws, varpath(I), ws, "}", ws, "</", tag(T), ">", ws. 

% varquery -> vartag | varpath varquery(T, I) --> vartag(T,I).
varquery(I) --> vartag(I); varpath(I).


% docpath -> "doc" "(" expr ")"
docpath(docpath(E)) --> ws, "doc", ws, "(", expr(E), ")", ws.

%sourcequery -> docpath | qvar
sourcequery(I) --> docpath(I) ; qvar(I). 

%exprquery -> sourcequery ( startxpath )? ;
exprquery(exprquery(I, P)) --> (sourcequery(I), startxpath(P);sourcequery(I)).

%forquery -> for qvar in exprquery return varquery;
forquery(forquery(V, I, P)) --> ws, "for", ws, qvar(V), ws, "in", ws, exprquery(I), ws, "return", ws, varquery(P). 

%tagquery < tag > { forquery } </tag>
tagquery(tagquery(T,I)) --> ws, "<", tag(T), ">", ws, "{", ws, forquery(I), ws, "}", ws,  "</", tag(T), ">", ws. 

%urquery -> tagquery;
urquery(I) --> tagquery(I).

letprog(letprog(I, E, U)) --> ws, "let", ws, id(I), ws, "=", ws, expr(E), ws, "in", ws, urquery(U), ws.
letprog(let(I, E)) --> ws, "let", ws, id(I), ws, "=", ws, expr(E), ws.

urquery_list([L | R]) --> (letprog(L); urquery(L); exprquery(L);varpath(L);docpath(L); startxpath(L);xpath(L);qvar(L)), urquery_list(R), {!}.
urquery_list([]) --> [].

prog_urquery(sequence(L)) --> urquery_list(L), {!}.
prog_urquery(none) --> [].

%%%%%%%%%%%%%%%%%%%%%%%%%% Lexer Xquery Utils %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(L) :- 
    File = 'test.txt',
    read_file_to_codes(File, Codes, []),
    atom_codes(Input, Codes),
    format('Input=~n~s~n', [Input]),
    phrase(prog_urquery(L), Codes)
.