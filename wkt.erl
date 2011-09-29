% This software is licensed under the MIT License
% Copyright (c) 2010 Volker Mische (http://vmx.cx/)

% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:

% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.

% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
% THE SOFTWARE.

% This code is heavily based on the ideas of the MochiWeb's JSON2 decoder
% http://code.google.com/p/mochiweb/source/browse/trunk/src/mochijson2.erl
% (Copyright 2006 Mochi Media, Inc.)


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
    C when ((C >= $0) and (C =< $9)) orelse (C == $-) orelse (C == $+) ->
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
        Acc2 = case tuple_them(Acc) of
        % Else points will end up as [[x, y]] instead if [x, y]
        [SingleItem] when is_number(hd(SingleItem)) ->
            lists:reverse(SingleItem);
        MultipleItems ->
            MultipleItems
        end,
        {{parsed_list, lists:reverse(Acc2)}, Wkt2};
    {space, Wkt2} ->
        parse_list_inner(Wkt2, Acc);
    {{parsed, Parsed}, Wkt2} ->
        parse_list_inner(Wkt2, [Parsed|Acc]);
    {comma, Wkt2} ->
        Acc2 = tuple_them(Acc),
        parse_list(Wkt2, Acc2)
    end.

% converts leading non-list elements to a list
% i.e. [a,b,[c,d],[e,f]] -> [[a,b],[c,d],[e,f]]
tuple_them(List) ->
    tuple_them(List, []).
% case when the comma is behind a parenthesis and not behind a number
tuple_them([], Acc) when is_list(hd(Acc)) ->
    lists:reverse(Acc);
tuple_them([], Acc) ->
    [Acc];
tuple_them([H|_T]=Rest, Acc) when is_tuple(H); is_list(H) ->
    case Acc of
        [] -> Rest;
        _ -> [Acc|Rest]
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

parse_number([H|T], float, Acc) when H == $E orelse H == $- orelse H == $+ ->
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
    {{Type2, String}, Wkt2} = case parse_string(T, [H]) of
    {{type, Type}, Wkt} ->
        {{parsed_atom, list_to_atom(Type)}, Wkt};
    {{empty_type, Type}, Wkt} ->
        {{parsed_empty, list_to_atom(Type)}, Wkt}
    end,
    % The WKT specification doesn't say anything about the case of the
    % letters, therefore we transform them to camel case style, like in
    % GeoJSON specification
    String2 = case String of
        point -> 'Point';
        multipoint -> 'MultiPoint';
        linestring -> 'LineString';
        multilinestring -> 'MultiLineString';
        polygon -> 'Polygon';
        multipolygon -> 'MultiPolygon';
        geometrycollection -> 'GeometryCollection';
        _ -> String
    end,
    {{Type2, String2}, Wkt2}.


parse_string([H|T], Acc) when ((H >= $a) and (H =< $z)) orelse
                            ((H >= $A) and (H =< $Z)) orelse
                            H == $\s ->
    parse_string(T, [H|Acc]);
parse_string(Wkt, Acc) ->
    Stripped = string:strip(Acc, left),
    case Stripped of
    "YTPME " ++ Geometry ->
        Stripped2 = string:strip(Geometry, left),
        {{empty_type, lists:reverse(string:to_lower(Stripped2))}, Wkt};
    _ ->
        {{type, lists:reverse(string:to_lower(Stripped))}, Wkt}
    end.


parse_number_test() ->
    ?assertEqual({'Point', [12, 13]}, parse("POINT(12 13)")),
    ?assertEqual({'Point', [12.0, 13]}, parse("POINT(12.0 13)")),
    ?assertEqual({'Point', [12.458, 13]}, parse("POINT(12.458 13)")),
    ?assertEqual({'Point', [12, 13.4712]}, parse("POINT(12 13.4712)")),
    ?assertEqual({'Point', [1280.0, 13]}, parse("POINT(12.8E2 13)")),
    ?assertEqual({'Point', [0.128, 13]}, parse("POINT(12.8E-2 13)")),
    ?assertEqual({'Point', [12.8, 132500.0]}, parse("POINT(12.8 13.25E4)")),
    ?assertEqual({'Point', [12.8, 0.001325]}, parse("POINT(12.8 13.25E-4)")),

    ?assertEqual({'Point', [12, -13]}, parse("POINT(+12 -13)")),
    ?assertEqual({'Point', [-12.0, 13]}, parse("POINT(-12.0 +13)")),
    ?assertEqual({'Point', [-12.458, 13]}, parse("POINT(-12.458 13)")),
    ?assertEqual({'Point', [12, -13.4712]}, parse("POINT(  +12  -13.4712)")),
    ?assertEqual({'Point', [-1280.0, 0.001325]},
                 parse("POINT(-12.8E2 +13.25E-4)")),
    ?assertEqual({'Point', [0.128, 132500.0]},
                 parse("POINT(+12.8E-2 13.25E+4)")),
    ?assertEqual({'Point', [0.128, -132500.0]},
                 parse("POINT(+12.8E-2 -13.25E+4)")).


parse_whitespace_empty_test() ->
    Point = {'Point', []},
    GC = {'GeometryCollection', [Point]},
    ?assertEqual(Point, parse("POINT EMPTY")),
    ?assertEqual(Point, parse("POINT  EMPTY")),
    ?assertEqual(Point, parse("POINT   EMPTY")),
    ?assertEqual(GC, parse("GEOMETRYCOLLECTION(POINT EMPTY)")),
    ?assertEqual(GC, parse("GEOMETRYCOLLECTION(POINT EMPTY )")),
    ?assertEqual(GC, parse("GEOMETRYCOLLECTION(POINT EMPTY  )")),
    ?assertEqual(GC, parse("GEOMETRYCOLLECTION(POINT  EMPTY  )")),
    ?assertEqual(GC, parse("GEOMETRYCOLLECTION(POINT   EMPTY  )")).

parse_whitespace_single_test() ->
    Point = {'Point', [12, 13]},
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
    LS = {'LineString', [[12, 13], [14, 15], [16, 17]]},
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
    GC = {'GeometryCollection', [{'Point', [12, 13]}]},
    GC2 = {'GeometryCollection', [{'Point', [12, 13]}, {'Point', [14, 15]}]},
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
    ?assertEqual({'Point', []}, parse("POINT EMPTY)")),
    ?assertEqual({'Point', [12, 13]}, parse("POINT (12 13)")).

parse_geom_linestring_test() ->
    ?assertEqual({'LineString', []},
                 parse("LINESTRING EMPTY")),
    ?assertEqual({'LineString', [[12, 13], [14, 15]]},
                 parse("LINESTRING (12 13, 14 15)")).

% the geometry of triangles are equal to ones of polygons
parse_geom_polygon_test() ->
    ?assertEqual({'Polygon', []},
                 parse("POLYGON EMPTY")),
    ?assertEqual({'Polygon', [[[12, 13], [24, 25], [36, 17], [12, 13]]]},
                 parse("POLYGON ((12 13, 24 25, 36 17, 12 13))")),
    ?assertEqual({'Polygon', [[[102, 103], [204, 205], [306, 107], [102, 103]],
                            [[12, 13], [24, 25], [36, 17], [12, 13]]]},
                 parse("POLYGON ((102 103, 204 205, 306 107, 102 103),"
                       "(12 13, 24 25, 36 17, 12 13))")),
    ?assertEqual({'Polygon', [[[102, 103], [204, 205], [306, 107], [102, 103]],
                            [[12, 13], [24, 25], [36, 17], [12, 13]],
                            [[62, 63], [74, 75], [86, 67], [62, 63]]]},
                 parse("POLYGON ((102 103, 204 205, 306 107, 102 103),"
                       "(12 13, 24 25, 36 17, 12 13),"
                       "(62 63, 74 75, 86 67, 62 63))")).

parse_geom_multipoint_test() ->
    ?assertEqual({'MultiPoint', []},
                 parse("MULTIPOINT EMPTY")),
    ?assertEqual({'MultiPoint', [[12, 13], [14, 15]]},
                 parse("MULTIPOINT (12 13, 14 15)")).

parse_geom_multilinestring_test() ->
    ?assertEqual({'MultiLineString', []},
                 parse("MULTILINESTRING EMPTY")),
    ?assertEqual({'MultiLineString', [[[12, 13], [14, 15]],
                                    [[16, 17], [18, 19]]]},
                 parse("MULTILINESTRING ((12 13, 14 15), (16 17, 18 19))")).

% the geometry of polyhedralsurfaces/tins are equal to ones of multipolygons
parse_geom_multipolygon_test() ->
    ?assertEqual({'MultiPolygon', []},
                 parse("MULTIPOLYGON EMPTY")),
    ?assertEqual({'MultiPolygon', [[[[12, 13], [24, 25], [36, 17], [12, 13]]],
                                 [[[2012, 2013], [2024, 2025],
                                   [2036, 2017], [2012, 2013]]]]},
                 parse("MULTIPOLYGON (((12 13, 24 25, 36 17, 12 13)),"
                       "((2012 2013, 2024 2025, 2036 2017, 2012 2013)))")),
    ?assertEqual({'MultiPolygon', [[[[102, 103], [204, 205], [306, 107], [102, 103]],
                                  [[12, 13], [24, 25], [36, 17], [12, 13]]],
                                 [[[2102, 2103], [2204, 2205],
                                   [2306, 2107], [2102, 2103]],
                                  [[2012, 2013], [2024, 2025],
                                   [2036, 2017], [2012, 2013]]]]},
                 parse("MULTIPOLYGON (((102 103, 204 205, 306 107, 102 103),"
                       "(12 13, 24 25, 36 17, 12 13)),"
                       "((2102 2103, 2204 2205, 2306 2107, 2102 2103),"
                       "(2012 2013, 2024 2025, 2036 2017, 2012 2013)))")).

parse_geom_geometrycollection_test() ->
    ?assertEqual({'GeometryCollection',[]},
                 parse("GEOMETRYCOLLECTION EMPTY")),
    ?assertEqual({'GeometryCollection',[{'Point', []}]},
                 parse("GEOMETRYCOLLECTION (POINT EMPTY)")),
    ?assertEqual({'GeometryCollection',[{'Point', []}, {'MultiPoint', []}]},
                 parse("GEOMETRYCOLLECTION (POINT EMPTY, MULTIPOINT EMPTY)")),
    ?assertEqual({'GeometryCollection',[{'Point', []},
                                      {'MultiPoint', [[12, 13], [14, 15]]}]},
                 parse("GEOMETRYCOLLECTION(POINT EMPTY,"
                       "MULTIPOINT (12 13, 14 15))")),

    ?assertEqual({'GeometryCollection',[{'Point', [12, 13]}]},
                 parse("GEOMETRYCOLLECTION(POINT (12 13))")),
    ?assertEqual({'GeometryCollection',[{'Point', [12, 13]},
                                      {'MultiPoint', [[12, 13], [14, 15]]}]},
                 parse("GEOMETRYCOLLECTION(POINT (12 13),"
                       "MULTIPOINT (12 13, 14 15))")),
    ?assertEqual({'GeometryCollection',
                  [{'Point', [12, 13]}, {'Point', [14, 15]},
                   {'Polygon', [[[102, 103], [204, 205], [306, 107], [102, 103]],
                              [[12, 13], [24, 25], [36, 17], [12, 13]],
                              [[62, 63], [74, 75], [86, 67], [62, 63]]]}]},
                 parse("GEOMETRYCOLLECTION("
                       "POINT (12 13),POINT (14 15),"
                       "POLYGON ((102 103, 204 205, 306 107, 102 103),"
                       "(12 13, 24 25, 36 17, 12 13),"
                       "(62 63, 74 75, 86 67, 62 63)))")).

% It's the same for all other geometries
parse_geom_with_space_test() ->
    ?assertEqual({'point z',[]}, parse("POINT Z EMPTY")),
    ?assertEqual({'point z',[]}, parse("POINT Z  EMPTY")),
    ?assertEqual({'point z',[12, 13, 14]},
                 parse("POINT Z (12 13 14)")),
    ?assertEqual({'point z',[12, 13, 14]},
                 parse("POINT Z  (12 13 14)")),
    ?assertEqual({'point zm',[]}, parse("POINT ZM EMPTY")),
    ?assertEqual({'point zm',[]}, parse("POINT ZM  EMPTY")),
    ?assertEqual({'point zm',[12, 13, 14, 15]},
                 parse("POINT ZM (12 13 14 15)")),
    ?assertEqual({'point zm',[12, 13, 14, 15]},
                 parse("POINT ZM  (12 13 14 15)")).
