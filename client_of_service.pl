/*
 Demo for consuming service from prolog
author: loriacarlos@gmail.com
since: 2022
*/
:- use_module(library(http/http_client)).


post(Reply) :-
       http_post('http://localhost:8000', 
                 atom('application/json', '{"a":1, "b":2}'), 
                 Reply, 
                 [method(post)]
                 )
.
