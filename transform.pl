/*
@about transform UrQuery into JS
@authors Grupo 3-1pm
@since 2022
*/

:- module(transform, [toJS/2]).

toJS(sequence(L), sequence(JS)) :-
   Comment = comment('Automatically converted'),
   UrQueryFunction = function(id(urquery_01), [], sequence(L)),
   Main = function(id(main)),
   CallMain = call(id(main), []),
   JS = [Comment, UrQueryFunction, Main, CallMain]
.