/*
Demo of lexing
@author loriacarlos@gmail.com
@since 2022
*/
:- module(lexing, [identifier/3, ws/2]).
:- use_module(library(dcg/basics)).
:- reexport(library(dcg/basics), [number/3]).

identifier(I) --> blanks, [C],{string(C);letter(C)}, rest_identifier(R), 
                  blanks, {atom_codes(I, [C|R])}.
rest_identifier([C|R]) --> [C], {string(C);letter(C);num(C)}, {!}, rest_identifier(R).
rest_identifier([]) --> [].

xml_identifier(I) --> blanks, [C],{string(C);letter(C)}, rest_xml_identifier(R), 
                  blanks, {atom_codes(I, [C|R])}.
rest_xml_identifier([C|R]) --> [C], {string(C);letter(C);num(C)}, {!}, rest_xml_identifier(R).
rest_xml_identifier([]) --> [].

expression(E) --> blanks, ("'";[34]), [C],{string(C)}, rest_expression(R), ("'";[34]), blanks, {atom_codes(E, [C|R])}.
rest_expression([C|R]) --> [C], {string(C)}, {!}, rest_expression(R).
rest_expression([]) --> [].

letter(C) :- code_type(C, alpha).
num(C) :- code_type(C, digit).
string(C) :- code_type(C, ascii).

is_id(A) :- atom_codes(A, Codes), phrase(identifier, Codes).


ws --> (" ";"\t";"\n";"\r"), ws.
ws -->  [].



