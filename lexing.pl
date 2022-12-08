/*
@about UrQuery lexer
@authors 
    natalia.ar969@gmail.com
    lufemoal19@gmail.com
@since 2022
*/
:- module(lexing, [identifier/3, ws/2]).
:- use_module(library(dcg/basics)).
:- reexport(library(dcg/basics), [number/3]).

identifier(I) --> blanks, [C],{is_period(C); string(C); letter(C); num(C)}, rest_identifier(R), 
                  blanks, {atom_codes(I, [C|R])}.
rest_identifier([C|R]) --> [C], {is_period(C); string(C); letter(C); num(C)}, {!}, rest_identifier(R).
rest_identifier([]) --> [].

letter(C) :- code_type(C, alpha).
num(C) :- code_type(C, digit).
string(C) :- code_type(C, ascii).
is_period(C)  :- code_type(C, period).
is_space(C)   :- code_type(C, space).
is_white(C)   :- code_type(C, white).

is_id(A) :- atom_codes(A, Codes), phrase(identifier, Codes).

ws --> (" ";"\t";"\n";"\r"), ws.
ws -->  [].