/*
@about transform UrQuery into JS
@authors 
    natalia.ar969@gmail.com
    lufemoal19@gmail.com
@since 2022
*/

:- module(transform, [toJS/2]).

toJS(sequence(L), sequence(JS)) :-
   Comment0 = comment('Se importa el modulo de soporte'),
   Import = import(args([ur_doc, ur_evaluate, ur_tag, ur_active_doc]),library('urquery.mjs')),
   Comment1 = comment('Se crea una funcion para el programa. Este es el query'),
   Main = function(id(main), [uri], sequence([let(id(uri), ('ur_active_doc()')), return('urquery_01(uri)')])),
   UrQueryFunction = function(id(urquery_01), [uri], sequence(L)),
   Comment2 = comment('Se crea una funcion main que representa el let y el query'),
   CallMain = call(id(main), []),
   JS = [Comment0, Import, Comment1, UrQueryFunction, Comment2,Main, CallMain]
.