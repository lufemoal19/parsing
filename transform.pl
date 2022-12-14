/*
@about transform UrQuery into JS
@authors Grupo 3-1pm
@since 2022
*/
   %Main = function(id(main)),
:- module(transform, [toJS/2]).
%[ur_doc, ur_evaluate, ur_tag, ur_active_doc]
toJS(sequence(L), sequence(JS)) :-
   Import = import(args([ur_doc, ur_evaluate, ur_tag, ur_active_doc]),library('urquery.mjs')),
   Comment = comment('Automatically converted'),
   Main = function(id(main), [uri], sequence([let(id(uri), ('ur_active_doc()')), return('urquery_01(uri)')])),
   UrQueryFunction = function(id(urquery_01), [uri], sequence(L)),
   CallMain = call(id(main), []),
   JS = [Import, Comment, UrQueryFunction, Main, CallMain]
.