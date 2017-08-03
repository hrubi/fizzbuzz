#!/usr/bin/env escript

% Distribute the fizzbuzz 'computation' to several workers.
% Demonstrates working with processes, no practical benefit here.

-module( fizzbuzzp ).

-mode( compile ).


% spawns workers and collect responses from them
fizzbuzzP( N, To ) ->
  lists:flatten(
    collect_responses(
      spawn_workers( chunks( N, To ) )
    )
  ).


% turns a list of { From, To } to a list of workers' Pids
spawn_workers( Chunks ) -> spawn_workers( Chunks, [] ).

spawn_workers( [], Workers ) -> Workers;
spawn_workers( [ { From, To } | Chunks ], Workers ) ->
  Parent = self(),
  Worker = spawn( fun() -> Parent ! { self(), fizzbuzz( From, To ) } end ),
  spawn_workers( Chunks, [ Worker | Workers ] ).


% turns a list of workers' Pids to a list of fizzbuzz results
collect_responses( Workers ) -> collect_responses( Workers, [] ).

collect_responses( [], L ) -> L;
collect_responses( [ Worker | Workers ], L ) ->
  Resp = receive
    { Worker, Result } -> Result
  end,
  collect_responses( Workers, [ Resp | L ] ).


% generic fizzbuzz computing From < X <= To
fizzbuzz( From, To ) -> fizzbuzz( From, To, [] ).

fizzbuzz( _Stop, _Stop, L ) -> L;
fizzbuzz( Stop, X, L ) when X rem 15 =:= 0 -> fizzbuzz( Stop, X - 1, [ 'FizzBuzz' | L ] );
fizzbuzz( Stop, X, L ) when X rem 3 =:= 0  -> fizzbuzz( Stop, X - 1, [ 'Fizz' | L ] );
fizzbuzz( Stop, X, L ) when X rem 5 =:= 0  -> fizzbuzz( Stop, X - 1, [ 'Buzz' | L ] );
fizzbuzz( Stop, X, L ) -> fizzbuzz( Stop, X - 1, [ X | L ] ).


% chunks( 3, 100 ) -> [ { 0, 34 }, { 34, 67 }, { 67, 100 } ]
chunks( N, To ) -> chunks( N, To, [] ).

chunks( 1, To, L ) -> [ { 0, To } | L ];
chunks( N, To, L ) ->
  From = To - ( To div N ),
  chunks( N - 1, From, [ { From, To } | L ] ).


main([ UpTo, Workers ]) ->
  N = list_to_integer( Workers ),
  To = list_to_integer( UpTo ),
  io:format("~p~n", [ fizzbuzzP( N, To ) ] );

main( _ ) -> usage().


usage() ->
  io:format("usage: ~s.erl up_to workers~n", [ ?MODULE ] ).
