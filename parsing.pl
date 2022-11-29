/*
Demo of parsing
@author loriacarlos@gmail.com
@since 2022
*/

:- use_module(lexing).
% 
main_urquery([L | R]) -->  ws, (prog_urquery(L);let(L);tagquery(L);forquery(L);exprquery(L);sourcequery(L);docpath(L);varquery(L);vartag(L);varpath(L);startxpath(L);xpath(L);qvar(L)), ws, main_urquery(R), {!}.
main_urquery([]) --> [].

let(let(X, E)) --> ws, "let", ws, id(X), ws, "=", ws, expr(E), ws.

id(id(I)) --> identifier(I).
xml_id(xml_id(I)) --> identifier(I).
num(num(N)) -->  number(N).
js_string(js_expr(I)) --> "'", identifier(I), "'".

expr(X) --> id(X);num(X);js_string(X).

% qvar: $variable
qvar(qvar(I)) --> ws, [36], id(I), ws.
tag(I) --> ws, xml_id(I), ws.

xpath(xpath(I, R)) --> tag(I), "/", xpath(R).
xpath(xpath(I)) --> tag(I). 
startxpath(startxpath(I)) --> "/", xpath(I).

%$galleta/sabor 
% gramatica varpath varpath ->qvar (startxpath)?;
varpath(varpath(I, P)) --> qvar(I), startxpath(P).

% vartag -> "<" tag ">" "{" varpath "}" "</" tag ">";
vartag(vartag(T, I)) --> ws, "<", tag(T), ">", "{",varpath(I), "}", "</", tag(T), ">", ws.  

% varquery -> vartag | varpath
varquery(varquery(I)) --> vartag(I); varpath(I).

% docpath -> "doc" "(" expr ")"
docpath(docpath(E)) --> ws, "doc", ws, "(", expr(E), ")", ws.

%sourcequery -> docpath | qvar
sourcequery(sourcequery(I)) --> docpath(I) ; qvar(I). 

%exprquery -> sourcequery ( startxpath )? ;
exprquery(exprquery(I, P)) --> sourcequery(I), startxpath(P).

%forquery -> for qvar in exprquery return varquery;
forquery(forquery(V, I, P)) --> ws, "for", ws, qvar(V), ws, "in", ws, exprquery(I), ws, "return", ws, varquery(P). 

%tagquery < tag > { forquery } </tag>
tagquery(tagquery(T,I)) --> ws, "<", tag(T), ">", ws, "{", ws, forquery(I), ws, "}", ws,  "</", tag(T), ">", ws. 

%urquery -> tagquery;
urquery(uq(I)) --> tagquery(I).

letprog(letprog(I, E, U)) --> ws, "let", ws, id(I), ws, "=", ws, expr(E), ws, "in", ws, urquery(U).

prog_urquery(prog(I)) --> letprog(I); urquery(I).
%%%%%%%%%%%%%%%%%%%%%%%%%% Lexer Xquery Utils %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(L) :- 
    File = 'test.txt',
    read_file_to_codes(File, Codes, []),
    atom_codes(Input, Codes),
    format('Input=~n~s~n', [Input]),
    phrase(main_urquery(L), Codes)
.