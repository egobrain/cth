%% Commont Tests Data helper
-module(ctdh).

-export([
    match/2,
    like/2
]).

-export([
    strict_diff/2,
    soft_diff/2
]).

-export([
    eq/1,

    map_like/1,
    map_equals/1,

    list_equals/1,
    list_contains_all/1,
    list_contains_exact/1,

    check/1,
    check/2,

    any/0
]).

-export([
    strict/1,
    soft/1,
    change_type/2
]).

%% =============================================================================
%% Ct API
%% =============================================================================

match(Expected, Given) ->
    Diff = strict_diff(Expected, Given),
    cth:assert(Diff =:= [],
        "given value is not equal to expected~n"
        "diff    : ~p", [Diff]).

like(Expected, Given) ->
    Diff = soft_diff(Expected, Given),
    cth:assert(Diff =:= [],
        "given value is not equal to expected~n"
        "diff    : ~p", [Diff]).

strict_diff(Expected, Given) ->
    diff(Expected, Given, fun strict/1).

soft_diff(Expected, Given) ->
    diff(Expected, Given, fun soft/1).

strict(M) when is_map(M) -> map_equals(M);
strict(L) when is_list(L) -> list_equals(L);
strict(V) -> eq(V).

soft(M) when is_map(M) -> map_like(M);
soft(L) when is_list(L) -> list_contains_all(L);
soft(V) -> eq(V).

%% =============================================================================
%% Diff API
%% =============================================================================

diff(Expected, Given, DiffF) -> lists:reverse(diff_(Expected, Given, [], [], DiffF)).
diff_(Fun, To, Path, Log, DiffF) when is_function(Fun) -> Fun(To, Path, Log, DiffF);
diff_(From, To, Path, Log, DiffF) -> diff_(DiffF(From), To, Path, Log, DiffF).

%% =============================================================================
%% Comparators
%% =============================================================================

eq(From) -> fun(To, Path, Log, _DiffF) ->
    case From =:= To of
        true -> Log;
        false -> [#{op => replace, path => path(Path), value => To}|Log]
    end
end.

map_equals(From) -> change_type(map_equals_(From), fun strict/1).
map_equals_(From) when is_map(From) ->
    fun (To, Path, Log, DiffF) when is_map(To) ->
            FromKeys = maps:keys(From),
            NewPairs = maps:to_list(maps:without(FromKeys, To)),
            {Log2, NewPairs2} = maps:fold(
                fun(K, FromV, {L, New}) ->
                    case maps:find(K, To) of
                        {ok, ToV} -> {diff_(FromV, ToV, [K|Path], L, DiffF), New};
                        error -> maybe_moved(K, FromV, NewPairs, Path, L)
                    end
                end, {Log, NewPairs}, From),
            lists:foldl(fun({K, V}, L) ->
                [#{op => add, path => path([K|Path]), value => V}|L]
            end, Log2, NewPairs2);
        (To, Path, Log, DiffF) -> (eq(From))(To, Path, Log, DiffF)
end.

map_like(From) when is_map(From) ->
    fun (To, Path, Log, DiffF) when is_map(To) ->
            FromKeys = maps:keys(From),
            Fun = change_type(map_equals_(From), fun soft/1),
            Fun(maps:with(FromKeys, To), Path, Log, DiffF);
        (To, Path, Log, DiffF) -> (eq(From))(To, Path, Log, DiffF)
    end.

check(Fun) -> fun(To, Path, Log, _DiffF) ->
    case Fun(To) of
        ok -> Log;
        {error, Reason} -> [#{op => check, path => path(Path), value => To, error => Reason}|Log]
    end
end.

check(BoolFun, Msg) ->
    check(fun(To) ->
        case BoolFun(To) of
            true -> ok;
            false -> {error, Msg}
        end
    end).

any() -> check(fun(_) -> ok end).

list_equals(From) when is_list(From) ->
    fun(To, Path, Log, DiffF) when is_list(To) -> list_diff_(From, To, Path, Log, DiffF, 0);
       (To, Path, Log, DiffF) -> (eq(From))(To, Path, Log, DiffF)
    end.

list_diff_([From|RestF], [To|RestT], Path, Log, DiffF, Cnt) ->
    list_diff_(RestF, RestT, Path, diff_(From, To, [Cnt|Path], Log, DiffF), DiffF, Cnt+1);
list_diff_([_|Rest], [], Path, Log, DiffF, Cnt) ->
    NewLog = [#{op => remove, path => path([Cnt|Path])}|Log],
    list_diff_(Rest, [], Path, NewLog, DiffF, Cnt+1);
list_diff_([], Rest, Path, Log, _DiffF, _Cnt) ->
    lists:foldl(fun(V, L) ->
        [#{op => add, path => path(["-"|Path]), value => V}|L]
    end, Log, Rest).

list_contains_all(From) when is_list(From) ->
    fun(To, Path, Log, DiffF) when is_list(To) -> list_contains_all_(From, To, Path, Log, DiffF, 0);
       (To, Path, Log, DiffF) -> (eq(From))(To, Path, Log, DiffF)
    end.

list_contains_all_([], _To, _Path, Log, _DiffF, _Cnt) -> Log;
list_contains_all_([H|T], To, Path, Log, DiffF, Cnt) ->
    NewLog = case take_first(fun(V) -> diff(H, V, DiffF) =:= [] end, To) of
        {_, Rest} -> Log;
        Rest -> [#{op => remove, path => path([Cnt|Path])}|Log]
    end,
    list_contains_all_(T, Rest, Path, NewLog, DiffF, Cnt + 1).

list_contains_exact(From) when is_list(From) ->
    fun(To, Path, Log, DiffF) when is_list(To) -> list_contains_exact_(From, To, Path, Log, DiffF, 0);
       (To, Path, Log, DiffF) -> (eq(From))(To, Path, Log, DiffF)
    end.

list_contains_exact_([], [], _Path, Log, _DiffF, _Cnt) -> Log;
list_contains_exact_([], To, Path, Log, _DiffF, _Cnt) ->
    lists:foldl(fun(V, L) ->
        [#{op => add, path => path(["-"|Path]), value => V}|L]
    end, Log, To);
list_contains_exact_([H|T], To, Path, Log, DiffF, Cnt) ->
    NewLog = case take_first(fun(V) -> diff(H, V, DiffF) =:= [] end, To) of
        {_, Rest} -> Log;
        Rest -> [#{op => remove, path => path([Cnt|Path])}|Log]
    end,
    list_contains_exact_(T, Rest, Path, NewLog, DiffF, Cnt + 1).

%% =============================================================================
%% Internal functions
%% =============================================================================

change_type(Fun, DiffFun) ->
    fun(To, Pairs, Path, _) -> Fun(To, Pairs, Path, DiffFun) end.

maybe_moved(K, FromV, Pairs, Path, L) ->
    maybe_moved_(K, FromV, Pairs, Path, L, []).
maybe_moved_(K, _V, [], Path, Log, Acc) ->
    {[#{op => remove, path => path([K|Path])}|Log], Acc};
maybe_moved_(K, V, [{NewK, V}|Rest], Path, Log, Acc) ->
    {[#{op => move, path => path([NewK|Path]), from => path([K|Path])}|Log],
     Acc ++ Rest};
maybe_moved_(K, V, [Other|Rest], Path, Log, Acc) ->
    maybe_moved_(K, V, Rest, Path, Log, [Other|Acc]).

path(Path) ->
    iolist_to_binary([["/", to_iodata(P)] || P <- lists:reverse(Path)]).

to_iodata(P) when is_atom(P) -> atom_to_list(P);
to_iodata(P) when is_integer(P) -> integer_to_list(P);
to_iodata(P) -> P.

take_first(Fun, List) -> take_first_(Fun, List, []).
take_first_(_Fun, [], Acc) -> lists:reverse(Acc);
take_first_(Fun, [H|T], Acc) ->
    case Fun(H) of
        true -> {H, lists:foldl(fun(A, L) -> [A|L] end, T, Acc)};
        false -> take_first_(Fun, T, [H|Acc])
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

diff_test_() ->
    [
     ?_test(?assertEqual([], strict_diff(#{a => 1}, #{a => 1}))),
     ?_test(?assertEqual(
         [
          #{ op => replace, path => <<"/a">>, value => 2}
         ],
         strict_diff(#{a => 1}, #{a => 2}))),
     ?_test(?assertEqual(
         [
          #{ op => move, path => <<"/b">>, from => <<"/a">>}
         ],
         strict_diff(#{a => 1}, #{b => 1}))),
     ?_test(?assertEqual(
         [
          #{ op => add, path => <<"/a">>, value => 1}
         ],
         strict_diff(#{}, #{a => 1}))),
     ?_test(?assertEqual(
         [
          #{ op => remove, path => <<"/a">>}
         ],
         strict_diff(#{a => 1}, #{}))),
     ?_test(?assertEqual(
         [
          #{op => remove,path => <<"/a">>},
          #{op => add,path => <<"/c">>,value => 3},
          #{op => add,path => <<"/b">>,value => 2}
         ],
         strict_diff(#{a => 1}, #{b => 2, c => 3}))),
     ?_test(?assertEqual(
         [
          #{op => move,path => <<"/e">>,from => <<"/a">>},
          #{op => replace,path => <<"/b/0">>,value => 2},
          #{op => replace,path => <<"/b/1/c">>,value => 4},
          #{op => add,path => <<"/b/-">>,value => 7},
          #{op => add,path => <<"/k">>,value => #{l => 1}}
         ],
         strict_diff(#{a => 1, b => [1, #{c => 3}], d => 4},
              #{e => 1, b => [2, #{c => 4}, 7], d => 4, k => #{l => 1}}))),

     ?_test(?assertEqual(
         [
          #{op => move,path => <<"/e">>,from => <<"/a">>},
          #{op => replace,path => <<"/b/0">>,value => 2},
          #{op => replace,path => <<"/b/1/c">>,value => 4},
          #{op => remove,path => <<"/b/2">>},
          #{op => add,path => <<"/k">>,value => #{l => 1}}
         ],
         strict_diff(#{a => 1, b => [1, #{c => 3}, 7], d => 4},
              #{e => 1, b => [2, #{c => 4}], d => 4, k => #{l => 1}})))
    ].

map_soft_strict_diff_test_() ->
    [
     ?_test(?assertEqual([], strict_diff(map_like(#{a => 1}), #{a => 1}))),
     ?_test(?assertEqual([], strict_diff(map_like(#{a => 1}), #{a => 1, b => 1}))),
     ?_test(?assertEqual([], strict_diff(#{a => map_like(#{b => 1})}, #{a => #{b => 1}}))),
     ?_test(?assertEqual(
         [
          #{op => replace,path => <<"/a">>,value => 1}
         ],
         strict_diff(map_like(#{a => 2}), #{a => 1, b => 1}))),
     ?_test(?assertEqual(
         [
          #{op => replace,path => <<"/a">>,value => 1}
         ],
         strict_diff(map_like(#{a => #{b => 1}}), #{a => 1, b => 1})))
    ].

list_contains_all_test_() ->
    [
     ?_test(?assertEqual([], strict_diff(list_contains_all([1,2,3]), [1,2,3]))),
     ?_test(?assertEqual([], strict_diff(list_contains_all([1,2,3]), [3,2,1]))),
     ?_test(?assertEqual([], strict_diff(list_contains_all([1,2,3]), [1,2,3,4]))),
     ?_test(?assertEqual([], strict_diff(list_contains_all([1,2,3]), [1,2,3,4]))),
     ?_test(?assertEqual([], strict_diff(list_contains_all([
         map_like(#{a => 1}),
         map_like(#{b => 2}),
         map_like(#{c => 3})
     ]), [#{a => 1}, #{ b => 2 }, #{ c => 3}, #{ d => 4}]))),
     ?_test(?assertEqual([
         #{op => remove,path => <<"/1">>},
         #{op => remove,path => <<"/2">>}
     ], strict_diff(list_contains_all([
         map_like(#{a => 1}),
         map_like(#{b => 2}),
         map_like(#{c => 3})
     ]), [#{a => 1, b => 2 , c => 3, d => 4}]))),
     ?_test(?assertEqual([
         #{op => remove, path => <<"/0">>},
         #{op => remove, path => <<"/1">>}
     ], strict_diff(list_contains_all([1,2,3]), [3])))
    ].

check_test_() ->
    IsBinary =
        fun(V) when is_binary(V) -> ok;
           (_) -> {error, <<"not is binary">>}
        end,
    [
     ?_test(?assertEqual([], strict_diff(check(IsBinary), <<"asd">>))),
     ?_test(?assertEqual([], strict_diff([check(IsBinary)], [<<"asd">>]))),
     ?_test(?assertEqual([], strict_diff([#{a => check(IsBinary)}], [#{a => <<"asd">>}]))),

     ?_test(?assertEqual([
         #{op => check, path => <<>>, value => 1, error => <<"not is binary">>}
     ], strict_diff(check(IsBinary), 1))),

     ?_test(?assertEqual([
         #{op => check, path => <<"/0/a">>, value => 1, error => <<"not is binary">>}
     ], strict_diff([#{a => check(IsBinary)}], [#{a => 1}])))
    ].

list_contains_exact_test_() ->
    [
     ?_test(?assertEqual([], strict_diff(list_contains_exact([1,2,3]), [1,2,3]))),
     ?_test(?assertEqual([], strict_diff(list_contains_exact([1,2,3]), [3,2,1]))),
     ?_test(?assertEqual([], strict_diff(list_contains_exact([
         map_like(#{a => 1}),
         map_like(#{b => 2}),
         map_like(#{c => 3})
     ]), [#{a => 1}, #{ b => 2 }, #{ c => 3}]))),

     ?_test(?assertEqual([
         #{op => add,path => <<"/-">>,value => 4}
     ], strict_diff(list_contains_exact([1,2,3]), [1,2,3,4]))),
     ?_test(?assertEqual([
         #{op => remove,path => <<"/1">>},
         #{op => remove,path => <<"/2">>},
         #{op => add,path => <<"/-">>,value => #{d => 4}}
     ], strict_diff(list_contains_exact([
         map_like(#{a => 1}),
         map_like(#{b => 2}),
         map_like(#{c => 3})
     ]), [#{a => 1, b => 2 , c => 3}, #{d => 4}]))),
     ?_test(?assertEqual([
         #{op => remove, path => <<"/0">>},
         #{op => remove, path => <<"/1">>}
     ], strict_diff(list_contains_exact([1,2,3]), [3])))
    ].

-endif.
