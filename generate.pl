/*
@about Generador JS
@authors 
    natalia.ar969@gmail.com
    lufemoal19@gmail.com
@since 2022
*/

:- module(generate, [response/2]).

:- use_module(parsing).
:- use_module(atom_stream).
:- use_module(transform).


generate_js_to_atom(JSAst, JSAtom) :-
  open_memory_outputstream(Handle, InMemoryStream),
  generate_js(JSAst, InMemoryStream),
  close(InMemoryStream),
  memory_file_to_atom(Handle, JSAtom),
  free_memory_file(Handle)
.

generate_js(sequence(L), Stream) :-
    forall(member(P, L), (generate_js(P, Stream), nl(Stream)) )
.

generate_js(import(args(L), library(D)), Stream) :-
    format(Stream, 'import {', []),
    generate_js_argslist(L, Stream),
    format(Stream, '} from ./~s;', [D])
.

generate_js(comment(C), Stream) :-
    format(Stream, '// ~s', [C])
.

generate_js(function(id(Name), Args, Body), Stream) :-
    format(Stream, 'function ~s(', [Name]),
    generate_js_argslist(Args, Stream),
    format(Stream, '){~n', []),
    generate_js(Body, Stream),
    format(Stream, '}~n', [])
.

generate_js(function*(id(Name), Args, Body), Stream) :-
    format(Stream, 'function* ~s(', [Name]),
    generate_js_argslist(Args, Stream),
    format(Stream, '){~n', []),
    generate_js(Body, Stream),
    format(Stream, '~n}~n', [])
.

generate_js(call(Expr, Args), Stream) :-
    generate_js(Expr, Stream),
    format(Stream, '(', []),
    generate_js_argslist(Args, Stream),
    format(Stream, ')', [])
.

generate_js(return(Expr), Stream) :-
    format(Stream, ' return ', []),
    generate_js(Expr, Stream),
    format(Stream, ';', [])
.

generate_js(return(T, Expr), Stream) :-
    format(Stream, '~n return ', []),
    generate_js(T, Stream),
    generate_js(Expr, Stream),
    format(Stream, ';', [])
.

generate_js(return, Stream) :-
    format(Stream, '~n return;', [])
.

generate_js(yield(Expr), Stream) :-
    format(Stream, '~t ~t yield ', []),
    generate_js(Expr, Stream),
    format(Stream, ';', [])
.

generate_js(spread(Expr), Stream) :-
    format(Stream, '...[', []),
    generate_js(Expr, Stream),
    format(Stream, ']', [])
.

generate_js(const(Left, Right), Stream) :-
    format(Stream, ' const ', []),
    generate_js(Left, Stream),
    format(Stream, ' = ', []),
    generate_js(Right, Stream),
    format(Stream, ';~n', [])
.

generate_js(let(Left, Right), Stream) :-
    format(Stream, ' let ', []),
    generate_js(Left, Stream),
    format(Stream, ' = ', []),
    generate_js(Right, Stream),
    format(Stream, ';', [])
.

generate_js(let(_, _, U), Stream) :- 
    generate_js(U, Stream)
.

generate_js(urquery(I), Stream):-
    generate_js(I, Stream)
.

generate_js(tagquery(T,I), Stream) :-
    generate_js(const(id_tag(T), lambda(tag(T))), Stream),
    generate_js(function*(id(for_01), [uri], I), Stream),
    generate_js(return(call(id_tag(T), [spread(call(for_01, [uri]))])), Stream)
.

generate_js(forquery(V, E, R), Stream) :-
    generate_js(const('xpath_result_iter', call(ur_evaluate, [call(ur_doc, [uri]), path(E)])), Stream),
    generate_js(const(R, lambda(R)), Stream),
    generate_js(for(V, R), Stream)
.

generate_js(for(V, R), Stream) :-
    format(Stream, '~t for (', []),
    generate_js(V, Stream),
    format(Stream, ' of xpath_result_iter){~n', []),
    generate_js(yield(call(R, [V])), Stream),
    format(Stream, '~n~t }', [])
.

generate_js(lambda(vartag(T, _)), Stream) :-
    format(Stream,'child => ur_tag("',[]),
    generate_js(T, Stream),
    format(Stream, '", child)', [])
.
generate_js(lambda(tag(T)), Stream) :-
    format(Stream,'children => ur_tag("',[]),
    generate_js(T, Stream),
    format(Stream, '", children)', [])
.

generate_js(exprquery(I, P), Stream) :- 
    generate_js(I, Stream),
    generate_js(P, Stream)
.

generate_js(path(exprquery(_, P)), Stream) :- 
    format(Stream, '"',[]),
    generate_js(P, Stream),
    format(Stream, '"',[])
.

generate_js(exprquery(I), Stream) :-
    generate_js(I, Stream)
.

generate_js(docpath(E), Stream) :- 
    generate_js(E, Stream)
.

generate_js(varpath(V, X), Stream) :- 
    generate_js(V, Stream),
    generate_js(X, Stream)
.

generate_js(vartag(T, _), Stream) :- generate_js(id_tag(T), Stream).

generate_js(qvar(I, R), Stream) :- 
    generate_js(I, Stream),
    generate_js(R, Stream)
.

generate_js(qvar(I), Stream) :- generate_js(I, Stream).

generate_js(xpath(I), Stream) :- 
    format(Stream, "/~s", I)
.

generate_js(xpath(I, R), Stream):-
    format(Stream, "/~s", I),
    generate_js(R, Stream)
.

generate_js(id(I), Stream)   :- format(Stream, '~s', I).
generate_js(tag(T), Stream) :- format(Stream, '~s',T).
generate_js(id_tag(tag(I)), Stream) :- format(Stream, '~s_tag', I).
generate_js(expr(E), Stream) :- format(Stream, '"~s"', E).
generate_js(num(N), Stream)  :- format(Stream, '~d', N).
generate_js(S, Stream)  :- format(Stream, '~s', S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  generate args list %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_js_argslist([], _).
generate_js_argslist([Arg], Stream) :- !,
    generate_js(Arg, Stream)
.

% para lista separada por comas
generate_js_argslist([Arg1, Arg2 | RestArgs], Stream) :-
    generate_js(Arg1, Stream),
    format(Stream, ', ', []),
    generate_js_argslist([Arg2 | RestArgs], Stream)
.


%%%%%%%%%% TESTS %%%%%%%%%%
test(JSAtom) :-
    test_00(JSAtom)
.

test_00(JSAtom) :-
    File = 'test_00.txt',
    read_file_to_codes(File, Codes, []),
    atom_codes(Input, Codes),
    format('Input = ~n~s~n', [Input]),
    phrase(prog_urquery(Prog), Codes),
    format('Ast from Input = ~q~n', [Prog]),
    toJS(Prog, JSProg),
    generate_js_to_atom(JSProg, JSAtom),
    format('Output = ~n~s~n', [JSAtom]), !  
.

test_01(JSAtom) :-
    File = 'test_01.txt',
    read_file_to_codes(File, Codes, []),
    atom_codes(Input, Codes),
    format('Input = ~n~s~n', [Input]),
    phrase(prog_urquery(Prog), Codes),
    format('Ast from Input = ~q~n', [Prog]),
    toJS(Prog, JSProg),
    generate_js_to_atom(JSProg, JSAtom),
    format('Output = ~n~s~n', [JSAtom]), !  
.

test_02(JSAtom) :-
    File = 'test_02.txt',
    read_file_to_codes(File, Codes, []),
    atom_codes(Input, Codes),
    format('Input = ~n~s~n', [Input]),
    phrase(prog_urquery(Prog), Codes),
    format('Ast from Input = ~q~n', [Prog]),
    toJS(Prog, JSProg),
    generate_js_to_atom(JSProg, JSAtom),
    format('Output = ~n~s~n', [JSAtom]), !  
.

test_03(JSAtom) :-
    File = 'test_03.txt',
    read_file_to_codes(File, Codes, []),
    atom_codes(Input, Codes),
    format('Input = ~n~s~n', [Input]),
    phrase(prog_urquery(Prog), Codes),
    format('Ast from Input = ~q~n', [Prog]),
    toJS(Prog, JSProg),
    generate_js_to_atom(JSProg, JSAtom),
    format('Output = ~n~s~n', [JSAtom]), !  
.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% RESPONSE TO SERVER %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

response(Request, JSAtom) :-
    atom_codes(Request, Codes),
    phrase(prog_urquery(Prog), Codes),
    toJS(Prog, JSProg),
    generate_js_to_atom(JSProg, JSAtom)
.