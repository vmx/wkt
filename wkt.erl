% The code is heavily based on MochiWeb's JSON decoder.

% Examples from Wikipedia article
% http://en.wikipedia.org/wiki/Simple_Features (2010-05-07)
% POINT (10 10)
% LINESTRING( 10 10, 20 20, 30 40)
% POLYGON ((10 10, 10 20, 20 20, 20 15, 10 10))
% MULTIPOINT(10 10, 20 20)
% MULTIPOLYGON(((10 10, 10 20, 20 20, 20 15, 10 10)),((60 60, 70 70, 80 60, 60 60)))
% GEOMETRYCOLLECTION(POINT (10 10), POINT(30 30), LINESTRING(15 15, 20 20))

% For OGC specification:
%MultiLineString((10 10, 20 20), (15 15, 30 15))


% TODO:
% parse empty lists ()

-module(wkt).
-include_lib("eunit/include/eunit.hrl").

-export([parse/1]).

parse(Wkt) ->
    case parse_char(Wkt) of
    {start_list, Wkt2} ->
        %{List, _Wkt} = parse_list(Wkt2),
        {{parsed_list, List}, _Wkt} = parse_list(Wkt2),
        List;
    {end_list, _Wkt} ->
io:format("parse: end_list~n", []),
        ok;
    {{parsed, Atom}, _Wkt} ->
        Atom;
    {{parsed_geom, Geom}, _Wkt} ->
        Geom;
    _ ->
        ok
    end.

%parse_char([]) ->

parse_char([H|T]=Wkt) ->
    io:format("parse: ~c|~p~n", [H, T]),
    case H of
    $( ->
        {start_list, T};
    $) ->
        {end_list, T};
    $, ->
        {comma, T};
    C when ((C >= $0) and (C =< $9)) ->
        parse_number(Wkt);
    C when ((C >= $a) and (C =< $z)) orelse ((C >= $A) and (C =< $Z)) ->
        %parse_atom(Wkt);
        parse_geometry(Wkt);
    $\s ->
        {space, T};
    _ ->
        parse_char(T)
    end.

parse_list(Wkt) ->
    %Foo = parse_list(Wkt, []),
    %lists:reverse(Foo)
%    {Res, _} = parse_list(Wkt, []),
%    lists:reverse(Res).
    parse_list(Wkt, []).

parse_list(Wkt, Acc) ->
    case parse_char(Wkt) of
    {end_list, Wkt2} ->
        io:format("parse_list: end_list: reverse: ~p ~p~n", [Acc, Wkt2]),
        {Acc, Wkt2};
    {start_list, Wkt2} ->
        {{parsed_list, List}, Wkt3} = parse_list(Wkt2),
        parse_list_inner(Wkt3, [List|Acc]);
    {{parsed, Parsed}, Wkt2} ->
        io:format("parse_list: parsed: ~p ~p~n", [Parsed, Acc]),
        parse_list_inner(Wkt2, [Parsed|Acc]);
    {{parsed_geom, Geom}, Wkt2} ->
        io:format("parse_list: parsed_geom: ~p ~p~n", [Geom, Acc]),
        %parse_list(Wkt2, [Geom|Acc]);
        parse_list_inner(Wkt2, [Geom|Acc]);
    {space, Wkt2} ->
        parse_list(Wkt2, Acc)
    end.

parse_list_inner(Wkt, Acc) ->
    io:format("parse_list_inner:~p ~p~n", [Acc, Wkt]),
    case parse_char(Wkt) of
    {end_list, Wkt2} ->
        io:format("parse_list_inner: done: ~p ~p~n", [Acc, Wkt2]),
        Acc2 = tuple_them(Acc),
        {{parsed_list, lists:reverse(Acc2)}, Wkt2};
    {space, Wkt2} ->
        parse_list(Wkt2, Acc);
    {comma, Wkt2} ->
        io:format("parse_list_inner: (1) comma:~p ~p~n", [Acc, Wkt2]),
        Acc2 = tuple_them(Acc),
        %Acc2 = Acc,
        parse_list(Wkt2, Acc2)
    end.

% converts leading non-tuple elements to a tuple
% i.e. [a,b,{c,d},{e,f}] -> [{a,b},{c,d},{e,f}]
tuple_them(List) ->
    tuple_them(List, []).
%tuple_them([], Acc) ->
%    io:format("tuple_them2: tail:~p ~n", [Acc]),
%    [list_to_tuple(Acc)];
% case when the comma is behind a parenthesis and not behind a number
tuple_them([], Acc) when is_list(hd(Acc)) ->
    io:format("tuple_them3: tail:~p ~n", [Acc]),
    lists:reverse(Acc);
tuple_them([], Acc) ->
    io:format("tuple_them2: tail:~p ~n", [Acc]),
    [list_to_tuple(Acc)];
tuple_them([H|_T]=Rest, Acc) when is_tuple(H) ->
%tuple_them([H|_T]=Rest, Acc) when is_tuple(H) and length(Acc)>=1 ->
    %if length(Acc)>0 -> io:format("    > 0:~p ~p ~p~n", [Acc, Rest, length(Acc)]);
    %    true -> io:format("    == 0:~p ~p ~p~n", [Acc, Rest, length(Acc)])
    %end,
    io:format(" tuple_them: tail:~p ~p ~p~n", [Acc, Rest, length(Acc)]),
    case Acc of
    [] ->
        Rest;
    _ ->
        [list_to_tuple(Acc)|Rest]
    end;
tuple_them([H|T], Acc) ->
    tuple_them(T, [H|Acc]).


parse_number([H|T]) ->
    io:format("parse_number:~c~n", [H]),
    case parse_number(T, [H]) of
    {{number, Num}, Wkt} ->
        %parse_number(T, [H] ++ [Num]);
        {{parsed, erlang:list_to_integer(Num)}, Wkt};
    {no_number} ->
        T
    end.

%parse_number([], Acc) ->
%    {number, Acc};
parse_number([H|T], Acc) when (H >= $0) and (H =< $9) ->
    %{number, H};
    parse_number(T, Acc ++ [H]);
% end of number/recursion
parse_number(Wkt, Acc) ->
    {{number, Acc}, Wkt}.



parse_geometry(Wkt) ->
    {{parsed, Atom}, Wkt2} = parse_atom(Wkt),
    %{{parsed_list, List}, Wkt3} = parse_geometry(Wkt2, []),
    case parse_geometry(Wkt2, []) of
    {{parsed_list, List}, Wkt3} ->
        {{parsed_geom, {Atom, List}}, Wkt3};
    {Geom, _Wkt3} ->
        io:format("parse_geometry:~p~n", [Geom]),
        Geom
    end.

parse_geometry(Wkt, Acc) ->
    case parse_char(Wkt) of
    {space, Wkt2} ->
        parse_geometry(Wkt2, Acc);
%    {comma, Wkt2} ->
%        io:format("parse_geometry: comma~p~n", [Acc]),
%        parse_geometry(Wkt2, Acc);
    {start_list, Wkt2} ->
        parse_list(Wkt2);
%        %{{parsed_list, List}, Wkt3} = parse_list(Wkt2);
%        parse_list_inner(Wkt2, Acc);
    Geometry ->
        Geometry
    end.


% all keywords (the geometry type) become Erlang atoms
parse_atom([H|T]) ->
    io:format("(1) atom:~p~n", [H]),
    {Atom, Wkt} = parse_atom(T, [H]),
    io:format("(2) atom:~p~n", [Atom]),
    {{parsed, list_to_atom(Atom)}, Wkt}.

parse_atom([H|T], Acc) when ((H >= $a) and (H =< $z)) orelse
                              ((H >= $A) and (H =< $Z)) ->
%    io:format("(1) parse_atom:~p~n", [H]),
    parse_atom(T, Acc ++ [H]);
parse_atom(Wkt, Acc) ->
%    io:format("(2) parse_atom:~p~n", [hd(Wkt)]),
    {Acc, Wkt}.

parse_test() ->
    %Result = parse("(10 11 13, 21 23 46, 47 58 69)"),
    %Result = parse("((10 11, 13 15), (21 23, 46 47), (47 58, 69 85))"),
    %Result = parse("((10 11))"),
    Result = parse("(((10 11)))"),
    io:format("Results: ~p~n", [Result]).


%parse_test() ->
%    Result1 = parse("point(10 10)"),
%    Result2 = parse("LINESTRING(10 10, 20 20, 30 40)"),
%    Result3 = parse("POLYGON ((10 11, 10 21, 20 21, 20 15, 10 11))"),
%    Result4 = parse("POLYGON ((0.0 0.0, 0.0 1.0, 1.0 1.0, 1.0 0.0, 0.0 0.0), (0.2 0.2, 0.2 0.8, 0.8 0.8, 0.8 0.2, 0.2 0.2))"),
%    Result5 = parse("MULTIPOLYGON(((10 10, 10 20, 20 20, 20 15, 10 10)),((60 60, 70 70, 80 60, 60 60)))"),
%    Result6 = parse("POLYGON ((0.0 0.0, 0.0 1.0), (0.2 0.2, 0.2 0.8), (0.5 0.5, 0.5 0.6))"),
%    io:format("Result1: ~p~n", [Result1]),
%    io:format("Result2: ~p~n", [Result2]),
%    io:format("Result3: ~p~n", [Result3]),
%    io:format("Result4: ~p~n", [Result4]),
%    io:format("Result5: ~p~n", [Result5]),
%    io:format("Result6: ~p~n", [Result6]).
%    %?assertEqual(something, Result).

%scan_test() ->
%    Result = scan("point(10 10)"),
    %Result = scan("point(10.5 -0.11)"),
    %Result = scan("LINESTRING(10 10, 20 20, 30 40)"),
    %Result = scan("POLYGON ((10 11, 10 21, 20 21, 20 15, 10 11))"),
    %p = Polygon(((0, 0), (0, 1), (1, 1), (1, 0)), [((0.2, 0.2), (0.2, 0.8), (0.8, 0.8), (0.8, 0.2))])
    %Result = scan("POLYGON ((0.0 0.0, 0.0 1.0, 1.0 1.0, 1.0 0.0, 0.0 0.0), (0.2 0.2, 0.2 0.8, 0.8 0.8, 0.8 0.2, 0.2 0.2))"),
    %Result = scan("POLYGON ((0.0 0.0, 0.0 1.0) , )"),
    %Result = scan("POLYGON ((0.0 0.0, 0.0 1.0), (0.2 0.2, 0.2 0.8), (0.5 0.5, 0.5 0.6))"),
    %p = Polygon(((0, 0), (0, 1), (1, 1), (1, 0)), [((0.2, 0.2), (0.2, 0.3), (0.3, 0.3), (0.3, 0.2)), ((0.5, 0.5), (0.5, 0.6), (0.6, 0.6), (0.6, 0.5))])
    %Result = scan("POLYGON ((0.0 0.0, 0.0 1.0, 1.0 1.0, 1.0 0.0, 0.0 0.0), (0.2 0.2, 0.2 0.3, 0.3 0.3, 0.3 0.2, 0.2 0.2), (0.5 0.5, 0.5 0.6, 0.6 0.6, 0.6 0.5, 0.5 0.5))"),
    %Result = scan("MULTIPOINT(10 10, 20 20)"),
    %Result = scan("MultiLineString((10 10, 20 20), (15 15, 30 15))"),
    %Result = scan("MULTIPOLYGON(((10 10, 10 20, 20 20, 20 15, 10 10)),((60 60, 70 70, 80 60, 60 60)))"),
%    Result = scan("GEOMETRYCOLLECTION(POINT (10 10), POINT(30 30), LINESTRING(15 15, 20 20))"),
    %Result = scan("G(10)"),
    %Result = scan("Gewrw(Pewr(10 10))"),
%    io:format("Results: ~p~n", [Result]).
    %?assertEqual(something, scan("point(10 10)")).

%scan_ws_test() ->
%    ?assertEqual({ok,[{text,{1,2},"p"}]}, scan(" p")),
%    %?assertEqual({ok,[{text,{1,2},"p"}]}, scan("p")),
%    Expected = {ok,[{text,{1,1},"point"},
%     {open_coords_list,{1,7},["["]},
%     {open_coords,{1,8},["["]},
%     {number_literal,{1,8},"10"},
%     {number_literal,{1,11},"11"},
%     {close_coords,{1,13},["]"]},
%     {close_coords_list,{1,13},["]"]}]},
%    ?assertEqual(Expected, scan("point (10 11)")),
%    Expected2 = {ok,[{text,{1,1},"LINESTRING"},
%     {open_coords_list,{1,11},["["]},
%     {open_coords,{1,12},["["]},
%     {number_literal,{1,12},"10"},
%     {number_literal,{1,15},"10"},
%     {close_coords,{1,17},["]"]},
%     {open_coords,{1,19},["["]},
%     {number_literal,{1,19},"20"},
%     {number_literal,{1,22},"20"},
%     {close_coords,{1,24},["]"]},
%     {close_coords_list,{1,24},["]"]}]},
%    io:format("~p", [scan("LINESTRING(10 10, 20 20)")]).
%    %?assertEqual(Expected2, scan("LINESTRING(10 10, 20 20)")),
%    %?assertEqual(Expected2, scan("LINESTRING(10 10,20 20)")).

%scan_minus_test() ->
%    Result = scan("point(-10 11)"),
%    ?assertEqual(something, Result).


%scan_parenthesis_test() ->
%    Result = scan("point(-10)"),
%    ?assertEqual(something, Result),
%    Result2 = scan("point(-10 11)"),
%    ?assertEqual(something, Result2),
%    Result3 = scan("point((-10 11)"),
%    ?assertEqual(something, Result3).
