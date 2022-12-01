/*
Service for adding two numbers
URI: /add
VERB: POST
Body 
    Expects:JSON {"urquery":urquery_code}
    
Returns: {"accepted": true, "answer": js_code}    if data ok
         {"accepted": false, "answer": "", "msg": some_error_message} othwerwise
             
author: loriacarlos@gmail.com
since: 2022
*/
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_log)).

:- use_module(library(http/html_write)).

:- use_module(generate).

% URL handlers.
:- http_handler('/add', handle_request, [method(post)]).
:- http_handler('/', home, []).



handle_request(Request) :-
    http_read_json_dict(Request, Query),
    solve(Query, Solution),
    reply_json_dict(Solution).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

set_setting(http:logfile, 'service_log_file.log').

%%%%%%%%%%%%%%%%%%%%%%%%%% BUSINESS LOGIC %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Calculates a + b.
solve(_{code: L}, _{status: true, answer:N, msg:'succeed'}) :-
    %atom_codes(L, Codes),
    response(L, JSAtom),
    N = JSAtom
.
solve(_{code: L}, _{accepted: false, answer:L, msg:'Error: failed syntax validation'}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

home(_Request) :-
        reply_html_page(title('Mini Add Service'),
                        [ h1('To use it:'),
                          p([h4('Send a post messsage'),
                             h4('URI:/add'),
                             h4('body: JSON data of the form {"a":number, "b":number}'),
                             h4('Service Responds with JSON as follows:'),
                             ul([li('{accepted:true, answer:a+b}    if data ok'),
                                 li('{accepted:false, answer:0, msg:some_error_message} othwerwise')])
                            ])
                        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MAIN %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- initialization
    format('*** Starting Server ***~n', []),
    (current_prolog_flag(argv, [SPort | _]) -> true ; SPort='8000'),
    atom_number(SPort, Port),
    format('*** Serving on port ~d *** ~n', [Port]),
    server(Port).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
extract_code_bytes(JSon, Bytes):-
    SourceCode = JSon.get(code),
	atom_codes(SourceCode, Bytes)
.

test_03 :-
   JSon = _{code:'//My first comment*\nlet d = "text.xml";\nlet a = "text2.xml";'},
   extract_code_bytes(JSon, Bytes),
   format('~s~n', [Bytes]),
   atom_codes(FromBytes, Bytes),
   Original = JSon.get(code),
   format('original ~n~s~n == ~n~s~n', [Original, FromBytes])
.