/*
@about Generador JS
@authors Grupo 3-1pm
@since 2022
*/

:- module(generate, [generate_js_to_atom/2]).

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

generate_js(comment(C), Stream) :-
    format(Stream, '// ~s~n', [C])
.

generate_js(function(id(Name), Args, Body), Stream) :-
    format(Stream, '~n function ~s(', [Name]),
    generate_js_argslist(Args, Stream),
    format(Stream, '){~n', []),
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

generate_js(forquery(V, E, R), Stream) :-
    format(Stream,'function * for_01(uri){~n~t',[]),
    format(Stream, 'const xpath_result_iter = ur_evaluate(ur_doc(uri,',[]),
    generate_js(E, Stream),
    format(Stream, ',)',[]),
    format(Stream, '~n', []),
    generate_js_lambda(R, Stream),
    format(Stream, '~n', []),

    format(Stream, 'for (', []),
    generate_js(V, Stream),
    format(Stream, ' of xpath_result_iter){~n', []),
    format(Stream, 'yield ',[]),
    generate_js(R, Stream),
    format(Stream, '_tag(',[]),
    generate_js(V, Stream),
    format(Stream, ')~n',[]),
    format(Stream, '}~n', [])
    %generate_js(function(V, E, R), Stream)
.
/*
    format(Stream, 'const ', []),
    generate_js(T, Stream),
    format(Stream, '_tag = child => ur_tag(',[]),
    generate_js(T, Stream),
    format(Stream,',child)',[])
*/
generate_js(vartag(T, _), Stream) :-
    generate_js(T, Stream)
.

generate_js(tag(T), Stream) :-
    format(Stream, '~s',T)
.

generate_js_lambda(vartag(T, _), Stream) :-
    format(Stream, 'const ',[]),
    generate_js(T, Stream),
    format(Stream,'_tag = child => ur_tag(',[]),
    generate_js(T, Stream),
    format(Stream, ',child)', [])
.

%'const xpath_result_iter = ur_evaluate(ur_doc(hola.xml/hi))'
generate_js(exprquery(I, P), Stream) :- 
    generate_js(I, Stream),
    generate_js(P, Stream)
.

generate_js(exprquery(I), Stream) :-
    generate_js(I, Stream)
.

generate_js(varpath(V, X), Stream) :- 
    generate_js(V, Stream),
    generate_js(X, Stream)
.

generate_js(docpath(E), Stream) :- 
    generate_js(E, Stream)
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
generate_js(expr(E), Stream) :- format(Stream, '"~s"', E).
generate_js(num(N), Stream)  :- format(Stream, '~d', N).

% const li_tag = child => ur_tag('li', child)
generate_js(tag(T), Stream)  :- format(Stream, 'const ~s_tag', T).




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
    File = 'test.txt',
    read_file_to_codes(File, Codes, []),
    atom_codes(Input, Codes),
    format('Input = ~n~s~n', [Input]),
    phrase(prog_urquery(Prog), Codes),
    format('Ast from Input = ~q~n', [Prog]),
    toJS(Prog, JSProg),
    generate_js_to_atom(JSProg, JSAtom),
    format('Output = ~n~s~n', [JSAtom])    
.