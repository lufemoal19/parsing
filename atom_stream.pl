/*
@about atom_stream 
@authors 
    natalia.ar969@gmail.com
    lufemoal19@gmail.com
@since 2022
*/
:- module(atom_stream, [open_memory_outputstream/2]).

open_memory_outputstream(Handle, InMemoryStream):-
  new_memory_file(Handle),
  open_memory_file(Handle, write, InMemoryStream)
.
    
produce_atom_from_stream(A) :-
  open_memory_outputstream(Handle, InMemoryStream),
  emit_something(InMemoryStream),
  close(InMemoryStream),
  memory_file_to_atom(Handle, A),
  free_memory_file(Handle)
.

emit_something(InMemoryStream) :-
    format(InMemoryStream, 'let x = ~s~n', [something])
.