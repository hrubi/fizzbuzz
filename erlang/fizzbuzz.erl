#!/usr/bin/env escript

-module( fizzbuzz ).

-mode( compile ).


fizzbuzz( X ) -> fizzbuzz( X, [] ).

fizzbuzz( 0, L ) -> L;
fizzbuzz( X, L ) when X rem 15 =:= 0 -> fizzbuzz( X - 1, [ 'FizzBuzz' | L ] );
fizzbuzz( X, L ) when X rem 3 =:= 0 -> fizzbuzz( X - 1, [ 'Fizz' | L ] );
fizzbuzz( X, L ) when X rem 5 =:= 0 -> fizzbuzz( X - 1, [ 'Buzz' | L ] );
fizzbuzz( X, L ) -> fizzbuzz( X - 1, [ X | L ] ).


main([ UpTo ] ) ->
  To = list_to_integer( UpTo ),
  io:format("~p~n", [ fizzbuzz( To ) ] );

main( _ ) -> usage().


usage() ->
  io:format("usage: ~s.erl up_to~n", [ ?MODULE ] ).
