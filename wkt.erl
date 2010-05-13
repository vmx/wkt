% The code is heavily based on MochiWeb's JSON decoder.

-module(wkt).
-include_lib("eunit/include/eunit.hrl").

-export([parse/1]).


parse(Wkt) ->
    case parse_char(Wkt) of
    {{parsed, Parsed}, _Wkt} ->
        Parsed;
    {space, Wkt2} ->
        parse(Wkt2);
    _ ->
        error
    end.

parse_char([H|T]=Wkt) ->
    case H of
    $( ->
        {start_list, T};
    $) ->
        {end_list, T};
    $, ->
        {comma, T};
    C when ((C >= $0) and (C =< $9)) orelse (C == $-)->
        parse_number(Wkt);
    C when ((C >= $a) and (C =< $z)) orelse ((C >= $A) and (C =< $Z)) ->
        parse_geometry(Wkt);
    $\s ->
        {space, T};
    _ ->
        parse_char(T)
    end.

parse_list(Wkt) ->
    parse_list(Wkt, []).

parse_list(Wkt, Acc) ->
    case parse_char(Wkt) of
    {start_list, Wkt2} ->
        {{parsed_list, List}, Wkt3} = parse_list(Wkt2),
        parse_list_inner(Wkt3, [List|Acc]);
    {{parsed, Parsed}, Wkt2} ->
        parse_list_inner(Wkt2, [Parsed|Acc]);
    {space, Wkt2} ->
        parse_list(Wkt2, Acc)
    end.


parse_list_inner(Wkt, Acc) ->
    case parse_char(Wkt) of
    {end_list, Wkt2} ->
        Acc2 = tuple_them(Acc),
        {{parsed_list, lists:reverse(Acc2)}, Wkt2};
    {space, Wkt2} ->
        %parse_list(Wkt2, Acc);
        parse_list_inner(Wkt2, Acc);
    {{parsed, Parsed}, Wkt2} ->
        parse_list_inner(Wkt2, [Parsed|Acc]);
    {comma, Wkt2} ->
        Acc2 = tuple_them(Acc),
        parse_list(Wkt2, Acc2)
    end.

% converts leading non-tuple elements to a tuple
% i.e. [a,b,{c,d},{e,f}] -> [{a,b},{c,d},{e,f}]
tuple_them(List) ->
    tuple_them(List, []).
% case when the comma is behind a parenthesis and not behind a number
tuple_them([], Acc) when is_list(hd(Acc)) ->
    lists:reverse(Acc);
tuple_them([], Acc) ->
    [list_to_tuple(Acc)];
tuple_them([H|_T]=Rest, Acc) when is_tuple(H) ->
    case Acc of
    [] ->
        Rest;
    _ ->
        [list_to_tuple(Acc)|Rest]
    end;
tuple_them([H|T], Acc) ->
    tuple_them(T, [H|Acc]).


parse_number([H|T]) ->
    {Num, Type, Wkt} = parse_number(T, int, [H]),
    case Type of
    int ->
        {{parsed, list_to_integer(Num)}, Wkt};
    float ->
        {{parsed, list_to_float(Num)}, Wkt}
    end.

parse_number([H|T], float, Acc) when H == $E orelse H == $- ->
    parse_number(T, float, [H|Acc]);
parse_number([H|T], Type, Acc) when (H >= $0) and (H =< $9) ->
    parse_number(T, Type, [H|Acc]);
parse_number([H|T], _Type, Acc) when H == $. ->
    parse_number(T, float, [H|Acc]);
parse_number(Wkt, Type, Acc) ->
    {lists:reverse(Acc), Type, Wkt}.


parse_geometry(Wkt) ->
    case parse_string(Wkt) of
    {{parsed_atom, Atom}, Wkt2} ->
        {{parsed_list, List}, Wkt3} = parse_geometry_inner(Wkt2),
        {{parsed, {Atom, List}}, Wkt3};
    {{parsed_empty, Atom}, Wkt2} ->
        {{parsed, {Atom, []}}, Wkt2}
    end.

parse_geometry_inner(Wkt) ->
    case parse_char(Wkt) of
    {space, Wkt2} ->
        parse_geometry_inner(Wkt2);
    {start_list, Wkt2} ->
        parse_list(Wkt2)
    end.


% all keywords (the geometry type) become Erlang atoms
parse_string([H|T]) ->
    %{String, Wkt} = parse_string(T, [H]),
    case parse_string(T, [H]) of
    {{type, Type}, Wkt} ->
        {{parsed_atom, list_to_atom(Type)}, Wkt};
    {{empty_type, Type}, Wkt} ->
        {{parsed_empty, list_to_atom(Type)}, Wkt}
    end.

parse_string([H|T], Acc) when ((H >= $a) and (H =< $z)) orelse
                            ((H >= $A) and (H =< $Z)) orelse
                            H == $\s ->
    parse_string(T, [H|Acc]);
parse_string(Wkt, Acc) ->
    Stripped = string:strip(Acc, left),
    try
        "YTPME " ++ Geometry = Stripped,
        Stripped2 = string:strip(Geometry, left),
        {{empty_type, lists:reverse(string:to_lower(Stripped2))}, Wkt}
    catch _:_ ->
        {{type, lists:reverse(string:to_lower(Stripped))}, Wkt}
    end.



parse_whitespace_empty_test() ->
    Point = {point, []},
    GC = {geometrycollection, [Point]},
    ?assertEqual(Point, parse("POINT EMPTY")),
    ?assertEqual(Point, parse("POINT  EMPTY")),
    ?assertEqual(Point, parse("POINT   EMPTY")),
    ?assertEqual(GC, parse("GEOMETRYCOLLECTION(POINT EMPTY)")),
    ?assertEqual(GC, parse("GEOMETRYCOLLECTION(POINT EMPTY )")),
    ?assertEqual(GC, parse("GEOMETRYCOLLECTION(POINT EMPTY  )")),
    ?assertEqual(GC, parse("GEOMETRYCOLLECTION(POINT  EMPTY  )")),
    ?assertEqual(GC, parse("GEOMETRYCOLLECTION(POINT   EMPTY  )")).

parse_whitespace_single_test() ->
    Point = {point, [{12, 13}]},
    ?assertEqual(Point, parse("POINT(12 13)")),
    ?assertEqual(Point, parse("POINT (12 13)")),
    ?assertEqual(Point, parse("POINT  (12 13)")),
    ?assertEqual(Point, parse("POINT( 12 13)")),
    ?assertEqual(Point, parse("POINT(  12 13)")),
    ?assertEqual(Point, parse("POINT(12  13)")),
    ?assertEqual(Point, parse("POINT(12 13 )")),
    ?assertEqual(Point, parse("POINT(12 13  )")),
    ?assertEqual(Point, parse("POINT(12  13  )")).

parse_whitespace_multi_test() ->
    LS = {linestring, [{12, 13}, {14, 15}, {16, 17}]},
    ?assertEqual(LS, parse("LINESTRING(12 13,14 15,16 17)")),
    ?assertEqual(LS, parse("LINESTRING(12 13, 14 15,16 17)")),
    ?assertEqual(LS, parse("LINESTRING(12 13,  14 15,16 17)")),
    ?assertEqual(LS, parse("LINESTRING(12 13,14 15, 16 17)")),
    ?assertEqual(LS, parse("LINESTRING(12 13,14 15,  16 17)")),
    ?assertEqual(LS, parse("LINESTRING(12 13, 14 15,  16 17)")),
    ?assertEqual(LS, parse("LINESTRING(12 13 ,14 15,16 17)")),
    ?assertEqual(LS, parse("LINESTRING(12 13  ,14 15,16 17)")),
    ?assertEqual(LS, parse("LINESTRING(12 13, 14 15 ,16 17)")),
    ?assertEqual(LS, parse("LINESTRING(12 13,  14 15  ,16 17)")),
    ?assertEqual(LS, parse("LINESTRING(12 13,  14 15  , 16 17)")),
    ?assertEqual(LS, parse("LINESTRING(12 13,  14 15  ,  16 17)")),
    ?assertEqual(LS, parse("LINESTRING(12 13 ,  14 15  ,16 17)")),
    ?assertEqual(LS, parse("LINESTRING(12 13  ,  14 15  ,16 17)")),
    ?assertEqual(LS, parse("LINESTRING(12 13,14 15,16 17 )")),
    ?assertEqual(LS, parse("LINESTRING(12 13,14 15,16 17  )")),
    ?assertEqual(LS, parse("LINESTRING(12 13,14 15, 16 17 )")),
    ?assertEqual(LS, parse("LINESTRING(12 13,14 15, 16 17  )")),
    ?assertEqual(LS, parse("LINESTRING(12 13,14 15,  16 17  )")).

parse_whitespace_geometrycollection_test() ->
    GC = {geometrycollection, [{point, [{12, 13}]}]},
    GC2 = {geometrycollection, [{point, [{12, 13}]}, {point, [{14, 15}]}]},
    ?assertEqual(GC, parse("GEOMETRYCOLLECTION( POINT(12 13))")),
    ?assertEqual(GC, parse("GEOMETRYCOLLECTION(  POINT(12 13))")),
    ?assertEqual(GC, parse("GEOMETRYCOLLECTION(POINT(12 13) )")),
    ?assertEqual(GC, parse("GEOMETRYCOLLECTION(POINT(12 13)  )")),
    ?assertEqual(GC2, parse("GEOMETRYCOLLECTION( POINT(12 13),POINT(14 15))")),
    ?assertEqual(GC2, parse("GEOMETRYCOLLECTION(  POINT(12 13),POINT(14 15))")),
    ?assertEqual(GC2, parse("GEOMETRYCOLLECTION(POINT(12 13),POINT(14 15))")),
    ?assertEqual(GC2, parse("GEOMETRYCOLLECTION(POINT(12 13), POINT(14 15))")),
    ?assertEqual(GC2, parse("GEOMETRYCOLLECTION(POINT(12 13),  POINT(14 15))")),
    ?assertEqual(GC2, parse("GEOMETRYCOLLECTION(POINT(12 13),POINT(14 15) )")),
    ?assertEqual(GC2, parse("GEOMETRYCOLLECTION(POINT(12 13),POINT(14 15)  )")),
    ?assertEqual(GC2, parse("GEOMETRYCOLLECTION(POINT(12 13) ,POINT(14 15))")),
    ?assertEqual(GC2, parse("GEOMETRYCOLLECTION(POINT(12 13)  ,POINT(14 15))")).


parse_geom_point_test() ->
    ?assertEqual({point, []}, parse("POINT EMPTY)")),
    ?assertEqual({point, [{12, 13}]}, parse("POINT (12 13)")).

parse_geom_linestring_test() ->
    ?assertEqual({linestring, []},
                 parse("LINESTRING EMPTY")),
    ?assertEqual({linestring, [{12, 13}, {14, 15}]},
                 parse("LINESTRING (12 13, 14 15)")).

% the geometry of triangles are equal to ones of polygons
parse_geom_polygon_test() ->
    ?assertEqual({polygon, []},
                 parse("POLYGON EMPTY")),
    ?assertEqual({polygon, [[{12, 13}, {24, 25}, {36, 17}, {12, 13}]]},
                 parse("POLYGON ((12 13, 24 25, 36 17, 12 13))")),
    ?assertEqual({polygon, [[{102, 103}, {204, 205}, {306, 107}, {102, 103}],
                            [{12, 13}, {24, 25}, {36, 17}, {12, 13}]]},
                 parse("POLYGON ((102 103, 204 205, 306 107, 102 103),"
                       "(12 13, 24 25, 36 17, 12 13))")),
    ?assertEqual({polygon, [[{102, 103}, {204, 205}, {306, 107}, {102, 103}],
                            [{12, 13}, {24, 25}, {36, 17}, {12, 13}],
                            [{62, 63}, {74, 75}, {86, 67}, {62, 63}]]},
                 parse("POLYGON ((102 103, 204 205, 306 107, 102 103),"
                       "(12 13, 24 25, 36 17, 12 13),"
                       "(62 63, 74 75, 86 67, 62 63))")).

parse_geom_multipoint_test() ->
    ?assertEqual({multipoint, []},
                 parse("MULTIPOINT EMPTY")),
    ?assertEqual({multipoint, [{12, 13}, {14, 15}]},
                 parse("MULTIPOINT (12 13, 14 15)")).

parse_geom_multilinestring_test() ->
    ?assertEqual({multilinestring, []},
                 parse("MULTILINESTRING EMPTY")),
    ?assertEqual({multilinestring, [[{12, 13}, {14, 15}],
                                    [{16, 17}, {18, 19}]]},
                 parse("MULTILINESTRING ((12 13, 14 15), (16 17, 18 19))")).

% the geometry of polyhedralsurfaces/tins are equal to ones of multipolygons
parse_geom_multipolygon_test() ->
    ?assertEqual({multipolygon, []},
                 parse("MULTIPOLYGON EMPTY")),
    ?assertEqual({multipolygon, [[[{12, 13}, {24, 25}, {36, 17}, {12, 13}]],
                                 [[{2012, 2013}, {2024, 2025},
                                   {2036, 2017}, {2012, 2013}]]]},
                 parse("MULTIPOLYGON (((12 13, 24 25, 36 17, 12 13)),"
                       "((2012 2013, 2024 2025, 2036 2017, 2012 2013)))")),
    ?assertEqual({multipolygon, [[[{102, 103}, {204, 205}, {306, 107}, {102, 103}],
                                  [{12, 13}, {24, 25}, {36, 17}, {12, 13}]],
                                 [[{2102, 2103}, {2204, 2205},
                                   {2306, 2107}, {2102, 2103}],
                                  [{2012, 2013}, {2024, 2025},
                                   {2036, 2017}, {2012, 2013}]]]},
                 parse("MULTIPOLYGON (((102 103, 204 205, 306 107, 102 103),"
                       "(12 13, 24 25, 36 17, 12 13)),"
                       "((2102 2103, 2204 2205, 2306 2107, 2102 2103),"
                       "(2012 2013, 2024 2025, 2036 2017, 2012 2013)))")).

parse_geom_geometrycollection_test() ->
    ?assertEqual({geometrycollection,[]},
                 parse("GEOMETRYCOLLECTION EMPTY")),
    ?assertEqual({geometrycollection,[{point, []}]},
                 parse("GEOMETRYCOLLECTION (POINT EMPTY)")),
    ?assertEqual({geometrycollection,[{point, []}, {multipoint, []}]},
                 parse("GEOMETRYCOLLECTION (POINT EMPTY, MULTIPOINT EMPTY)")),
    ?assertEqual({geometrycollection,[{point, []},
                                      {multipoint, [{12, 13}, {14, 15}]}]},
                 parse("GEOMETRYCOLLECTION(POINT EMPTY,"
                       "MULTIPOINT (12 13, 14 15))")),

    ?assertEqual({geometrycollection,[{point, [{12, 13}]}]},
                 parse("GEOMETRYCOLLECTION(POINT (12 13))")),
    ?assertEqual({geometrycollection,[{point, [{12, 13}]},
                                      {multipoint, [{12, 13}, {14, 15}]}]},
                 parse("GEOMETRYCOLLECTION(POINT (12 13),"
                       "MULTIPOINT (12 13, 14 15))")),
    ?assertEqual({geometrycollection,
                  [{point, [{12, 13}]}, {point, [{14, 15}]},
                   {polygon, [[{102, 103}, {204, 205}, {306, 107}, {102, 103}],
                              [{12, 13}, {24, 25}, {36, 17}, {12, 13}],
                              [{62, 63}, {74, 75}, {86, 67}, {62, 63}]]}]},
                 parse("GEOMETRYCOLLECTION("
                       "POINT (12 13),POINT (14 15),"
                       "POLYGON ((102 103, 204 205, 306 107, 102 103),"
                       "(12 13, 24 25, 36 17, 12 13),"
                       "(62 63, 74 75, 86 67, 62 63)))")).

% It's the same for all other geometries
parse_geom_with_space_test() ->
    ?assertEqual({'point z',[]}, parse("POINT Z EMPTY")),
    ?assertEqual({'point z',[]}, parse("POINT Z  EMPTY")),
    ?assertEqual({'point z',[{12, 13, 14}]},
                 parse("POINT Z (12 13 14)")),
    ?assertEqual({'point z',[{12, 13, 14}]},
                 parse("POINT Z  (12 13 14)")),
    ?assertEqual({'point zm',[]}, parse("POINT ZM EMPTY")),
    ?assertEqual({'point zm',[]}, parse("POINT ZM  EMPTY")),
    ?assertEqual({'point zm',[{12, 13, 14, 15}]},
                 parse("POINT ZM (12 13 14 15)")),
    ?assertEqual({'point zm',[{12, 13, 14, 15}]},
                 parse("POINT ZM  (12 13 14 15)")).
