/*
Demo of parsing
@author loriacarlos@gmail.com
@since 2022
*/

:- use_module(lexing).


urquery([L | R]) -->  ws, (let(L);varpath(L);startxpath(L);xpath(L);qvar(L)), ws, urquery(R), {!}.
urquery([]) --> [].

let(let(X, E)) --> ws, "let", ws, id(X), ws, "=", ws, expr(E), ws.

id(id(I)) --> identifier(I).
xml_id(xml_id(I)) --> identifier(I).
num(num(N)) -->  number(N).
js_string(js_expr(I)) --> "'", identifier(I), "'".

expr(X) --> id(X);num(X);js_string(X).

% qvar: $variable
qvar(id(I)) --> ws, [36], identifier(I), ws.
tag(I) --> ws, xml_id(I), ws.

%/catalogo/producto/nombre
%xpath "/" tag(I)
xpath(x_id(I)) --> (tag(I) ; "#", xpath(I)). 
startxpath(xpath(I)) --> "/", xpath(I).

%$galleta/sabor 
%varpath(xml_id(I)) --> qvar(I), "(", startxpath(I), ")", "?".
varpath(varpath(I, P)) --> qvar(I), startxpath(P).

% vartag -> "<" tag ">" "{" varpath "}" "</" tag ">";
vartag(vartag(xml_id(T),id(I), xml_id(P))) --> ws, "<", tag(T), ">", "{", 
    varpath(I, P), "}", "</", tag(T), ">", ws.  

%%%%%%%%%%%%%%%%%%%%%%%%%% Lexer Xquery Utils %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(L) :- 
    File = 'test.txt',
    read_file_to_codes(File, Codes, []),
    atom_codes(Input, Codes),
    format('Input=~n~s~n', [Input]),
    phrase(urquery(L), Codes)
.