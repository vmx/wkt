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

-export([scan/1]).

scan(Wkt) ->
    % WKT always starts with a geometry type
    scan(Wkt, [], {1, 1}, type).

%scan([], Scanned, _, _) ->
%    {ok, lists:reverse(lists:map(fun
%        ({identifier, Pos, String}) ->
%            RevString = lists:reverse(String),
%            Keywords = [ "if", "else", "endif", "not" ], %many others too
%            Type = case lists:member(RevString, Keywords) of
%            true ->
%                list_to_atom(RevString ++ "_keyword");
%            _ ->
%                identifier
%            end,
%            {Type, Pos, RevString};
%        ({Type, Pos, String}) ->
%            {Type, Pos, lists:reverse(String)}
%        end, Scanned))};

scan([], Scanned, _, _) ->
    {ok, lists:reverse(lists:map(fun({Type, Pos, String}) ->
        {Type, Pos, lists:reverse(String)}
    end, Scanned))};

% coord_list are all coordinates within a (). The second element in the
%     in_coord_list tuple is the number of parenthesis
% coord are digits that describe one coordinate.

% skip leading whitespaces
scan(" " ++ T, Scanned, {Row, Column}, type) ->
    scan(T, Scanned, {Row, Column+1}, type);

% get started
scan("(" ++ T, Scanned, {Row, Column}, type) ->
    scan(T, [{open_coords_list, {Row, Column}, ["["]}|Scanned], {Row, Column+1}, {in_coord_list, 1});
% opening further parenthesis
scan("(" ++ T, Scanned, {Row, Column}, {in_coord_list, Parenthesis}) ->
    scan(T, [{open_coords_list, {Row, Column}, ["["]}|Scanned], {Row, Column+1}, {in_coord_list, Parenthesis+1});

% closing after a parenthesis
scan(")" ++ T, Scanned, {Row, Column}, {in_coord_list, 1}) ->
    scan(T, [{close_coords_list, {Row, Column}, ["]"]}|Scanned], {Row, Column+1}, done);
scan(")" ++ T, Scanned, {Row, Column}, {in_coord_list, 2}) ->
    scan(T, [{close_coords_list, {Row, Column}, ["]"]}|Scanned], {Row, Column+1}, {in_coord_list, 1});

% closing after a coord
scan(")" ++ T, Scanned, {Row, Column}, {in_number, 1}) ->
    scan(T, [{close_coords_list, {Row, Column}, ["]"]}] ++
            [{close_coords, {Row, Column}, ["]"]}] ++
            Scanned,
         {Row, Column+1}, done);
scan(")" ++ T, Scanned, {Row, Column}, {in_number, Parenthesis}) ->
    scan(T, [{close_coords_list, {Row, Column}, ["]"]}] ++
            [{close_coords, {Row, Column}, ["]"]}] ++
            Scanned,
         {Row, Column+1}, {in_coord_list, Parenthesis-1});

% seperator within a coord
scan(" " ++ T, Scanned, {Row, Column}, {in_number, Parenthesis}) ->
    scan(T, Scanned, {Row, Column+1}, {in_coord, Parenthesis});

% seperator between coords
scan("," ++ T, Scanned, {Row, Column}, {in_number, Parenthesis}) ->
    scan(T, [{close_coords, {Row, Column}, ["]"]} | Scanned], {Row, Column+1}, {in_coord_list, Parenthesis});

% seperator between coords_list
scan("," ++ T, Scanned, {Row, Column}, {in_coord_list, Parenthesis}) ->
    scan(T, Scanned, {Row, Column+1}, {in_coord_list, Parenthesis});

% skip whitespace
scan(" " ++ T, Scanned, {Row, Column}, {in_coord, Parenthesis}) ->
    scan(T, Scanned, {Row, Column+1}, {in_coord, Parenthesis});
scan(" " ++ T, Scanned, {Row, Column}, {in_coord_list, Parenthesis}) ->
    scan(T, Scanned, {Row, Column+1}, {in_coord_list, Parenthesis});

% scanning text (geometry types)
scan([H|T], Scanned, {Row, Column}, type) ->
    scan(T, append_text_char(Scanned, {Row, Column}, H), {Row, Column+1}, type);

% numbers
scan([H | T], Scanned, {Row, Column}, {in_coord, Parenthesis}) ->
    CharType = char_type(H),
    if (CharType == digit) or (CharType == minus) ->
        scan(T,  [{number_literal, {Row, Column}, [H]} | Scanned], {Row, Column+1}, {in_number, Parenthesis});
    true ->
        {error, {illegal_character, {line, Row}, {column, Column}, {in_coord, Parenthesis}}}
    end;
scan([H | T], Scanned, {Row, Column}, {in_coord_list, Parenthesis}) ->
    CharType = char_type(H),
    if (CharType == digit) or (CharType == minus) ->
        scan(T, [{number_literal, {Row, Column}, [H]}] ++
                [{open_coords, {Row, Column}, ["["]}] ++
                Scanned,
             {Row, Column+1},{in_number, Parenthesis});
    % In case of a geometrycollection
    (CharType == letter) ->
        scan(T, append_text_char(Scanned, {Row, Column}, H), {Row, Column+1}, {in_coord_list, Parenthesis});
    true ->
        {error, {illegal_character, {line, Row}, {column, Column}, {in_coord_list, Parenthesis}}}
    end;
scan([H | T], Scanned, {Row, Column}, {in_number, Parenthesis}) ->
    CharType = char_type(H),
    if (CharType == digit) or (CharType == point) ->
        scan(T, append_char(Scanned, H), {Row, Column+1}, {in_number, Parenthesis});
    true ->
        {error, {illegal_character, {line, Row}, {column, Column}, {in_number, Parenthesis}}}
    end.



append_char(Scanned, Char) ->
    [String | Scanned1] = Scanned,
    [setelement(3, String, [Char | element(3, String)]) | Scanned1].

append_text_char(Scanned, {Row, Column}, Char) ->
    case length(Scanned) of
        0 ->
            [{text, {Row, Column}, [Char]}];
        _ ->
            [Token | Scanned1] = Scanned,
            case element(1, Token) of
                text ->
                    [{text, element(2, Token), [Char | element(3, Token)]} | Scanned1];
                _ ->
                    [{text, element(2, Token), [Char]} | Scanned]
            end
    end.

char_type(Char) ->
    case Char of 
        C when ((C >= $a) and (C =< $z)) or ((C >= $A) and (C =< $Z)) ->
            letter;
        C when ((C >= $0) and (C =< $9)) ->
            digit;
        C when (C == $.) ->
            point;
        C when (C == $-) ->
            minus;
        _ ->
            undefined
    end.



%scan_test() ->
    %Result = scan("point(10 10)"),
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
