% This Well Known Text (WKT) parser is based on the template compiling
% tutorial at
% http://www.evanmiller.org/write-a-template-compiler-for-erlang.html
% and the code at
% http://code.google.com/p/erlydtl/source/browse/trunk/src/erlydtl/erlydtl_scanner.erl
% (2010-05-07)


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


-module(wkt).
-include_lib("eunit/include/eunit.hrl").

-export([parse/1]).

%char_type(Char) ->
%    case Char of 
%        C when ((C >= $a) and (C =< $z)) or ((C >= $A) and (C =< $Z)) ->
%            letter;
%        C when ((C >= $0) and (C =< $9)) ->
%            digit;
%        C when (C == $.) ->
%            point;
%        C when (C == $-) ->
%            minus;
%        _ ->
%            undefined
%    end.


%parse(Wkt) ->
%    {ok, Scanned} = scan(Wkt),
%    io:format("Scanned: ~p~n", [Scanned]),
%    {[Type|Values], _} = lists:foldl(fun(Token, {Result, Stack}=Acc) ->
%        case Token of
%        {text, _, Text} ->
%            {Result ++ [Text], Stack};
%        {number_literal, _, Number} ->
%            [H|T] = Stack,
%            {Result, [H ++ [Number]|T]};
%        {open_coords, _, _} ->
%            {Result, [[]|Stack]};
%        {close_coords_list, _, _} when length(Stack) > 0 ->
%            {Result ++ [lists:reverse(Stack)], []};
%        _ ->
%            Acc
%        end
%    end, {[], []}, Scanned),
%    {Type, Values}.



parse([H|T]=Wkt) ->
    io:format("parse: ~c (~p)~n", [H, T]),
    case H of
%    <<_:O/binary, "(", _/binary>> ->
%    <<_:0/binary, "(", _/binary>> ->
    $( ->
        %{T, Acc} = parse_coord_list(T),
        %Acc;
        parse_coord_list(T);
%    $) ->
%        {end_coord_list};
%    C when ((C >= $0) and (C =< $9)) ->
%        %parse_number(Wkt);
%        {number, C};
    _ ->
        parse(T)
    end.


parse_coord_list(Wkt) ->
    parse_coord_list(Wkt, [[]]).
    %parse_coord_list(Wkt, []).

%coord_list([H|T], Acc) ->
parse_coord_list([H|T], Acc) ->
    io:format("parse_coord_list:~c~n", [H]),
    case H of
    $) ->
        %Acc2 = lists:reverse(Acc),
        %parse_coord_list(T, Acc2);
        {T, lists:reverse(Acc)};
    H when ((H >= $0) and (H =< $9)) ->
        %coord_list(Wkt, Acc ++ [Num])
        {Num, Wkt} = parse_number(T, [H]),
        [AccH|AccT] = Acc,
        parse_coord_list(Wkt, [AccH ++ [Num]|AccT]);
        %parse_coord_list(Wkt, [Num|Acc]);
    $\s ->
        parse_coord_list(T, Acc);
    $, ->
        io:format("$,:~p~n", [Acc]),
        % right a lsit of coords
        parse_coord_list(T, [[]|Acc]);
        %parse_coord_list(T, Acc);
    $( ->
        %[parse_coord_list(T, Acc)|[]];
        io:format("$(1:~p~n", [Acc]),
        %parse_coord_list(T, parse_coord_list(T, Acc));
        {T2, Cl} = parse_coord_list(T),
        io:format("$(2:~p~n", [Cl]),
        [AccH|AccT] = Acc,
        parse_coord_list(T2, [AccH ++ Cl|AccT]);
        %parse_coord_list(T2, [Cl|Acc]);
    _ ->
        io:format("parse_coord_list: otherwise:~n~c~n", [H])
    end.

%parse_number([H|T]) ->
%    case parse_number(T, [H]) of
%    {number, Num} ->
%        parse_number(T, [H] ++ [Num]);
%    {no_number} ->
%        T
%    end.


%parse_number([], Acc) ->
%    {number, Acc};
parse_number([H|T], Acc) when (H >= $0) and (H =< $9) ->
    %{number, H};
    parse_number(T, Acc ++ [H]);
parse_number(Wkt, Acc) ->
%    {no_number}.
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
