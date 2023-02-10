/*
@about prolog_server 
    simple web service --> localhost:8000
@authors 
    natalia.ar969@gmail.com
    lufemoal19@gmail.com
@since 2022
*/
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_log)).
:- use_module(library(http/html_write)).

:- use_module(generate).

% URL handlers.
:- http_handler('/compile', handle_request, [method(post)]).
:- http_handler('/', home, []).

handle_request(Request) :-
    http_read_json_dict(Request, Query),
    compiler(Query, Solution),
    reply_json_dict(Solution).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

set_setting(http:logfile, 'service_log_file.log').

%%%%%%%%%%%%%%%%%%%%%%%%%% BUSINESS LOGIC %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Transcompiler of urquery code into javascript code.
compiler(_{code: L}, _{status: true, answer:js_code, msg:'succeed'}) :-
    response(L, JSAtom),
    js_code = JSAtom
.
compiler(_{code: L}, _{accepted: false, answer:L, msg:'Error: failed syntax validation'}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

home(_Request) :-
    reply_html_page(title('Transcompiler Service'),
    [ h1('To use it:'),
        p([h4('Send urquery code'),
            h4('URI:/compile'),
            h4('request body: JSON data of the form {"code": urquery code as a string}'),
            h4('Service Responds with JSON as follows:'),
            ul([li('{code: JS code, accepted:true, answer:a+b, , msg:succeed}    if data is ok'),
                li('{accepted:false, answer:0, msg:some_error_message} othwerwise')])
        ])
    ])
.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MAIN %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- initialization
    format('*** Starting Server ***~n', []),
    (current_prolog_flag(argv, [SPort | _]) -> true ; SPort='8000'),
    atom_number(SPort, Port),
    format('*** Serving on port ~d *** ~n', [Port]),
    server(Port).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%