/*
@about Generador JS
@author Grupo 3-1pm
@since 2022
*/

:- module(generate, [generate_js_to_atom/2]).

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
generate_js(function(id(Name), Args, Body), Stream) :-
    format(Stream, '~n function ~s(', [Name]),
    generate_js_argslist(Args, Stream),
    format(Stream, '){', []),
    generate_js(Body, Stream),
    format(Stream, '}~n', [])
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

generate_js(return, Stream) :-
    format(Stream, ' return;', [])
.
generate_js(let(Left, Right), Stream) :-
    format(Stream, 'let ', []),
    generate_js(Left, Stream),
    format(Stream, ' = ', []),
    generate_js(Right, Stream),
    format(Stream, ';', [])
.

generate_js(id(I), Stream)   :- format(Stream, '~s', I).
generate_js(num(N), Stream)  :- format(Stream, '~d', N).

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
test_using_parsing(JSAtom) :-
    File = 'test.txt',
    read_file_to_codes(File, Codes, []),
    atom_codes(Input, Codes),
    format('Input=~n~s~n', [Input]),
    phrase(main_urquery(Prog), Codes),
    format('Ast from Input=~q~n', [Prog]),
    toJS(Prog, JSProg),
    generate_js_to_atom(JSProg, JSAtom),
    format('Output=~n~s~n', [JSAtom])    
.