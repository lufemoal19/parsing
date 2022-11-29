:- module(transform, [toJS/2]).

toJS(sequence(L), sequence(JS)) :-
   Comment = comment('Automatically Converted'),
   MainFunction = function(id(main), [], sequence(L)),
   CallMain = call(id(main), []),
   JS = [Comment, MainFunction, CallMain]
.