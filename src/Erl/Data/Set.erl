-module(erl_data_set@foreign).

-export([fromList_/1, empty_/0, isEmpty_/1, singleton_/1, toList_/1, eq_/2, union_/1]).

fromList_(L) ->
  sets:from_list(L, [{version, 2}]).

empty_() ->
  fromList_([]).

isEmpty_(S) ->
  sets:is_empty(S).

singleton_(E) ->
  fromList_([E]).

toList_(S) ->
  sets:to_list(S).

eq_(S1, S2) ->
  S1 =:= S2.

union_(Sets) ->
  sets:union(Sets).
