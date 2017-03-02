% To run the tests, type in erl repl:
% c(assignment), c(assignment_tests), eunit:test(assignment).

-module(assignment_tests).
-include_lib("eunit/include/eunit.hrl").

% helper function
between(X,Min,Max) -> (X > Min) and (X < Max).

% perimeter tests
perimeter_circle_test() ->
ExpectedCirclePerimeter = math:pi() * 2,
ExpectedCirclePerimeter = assignment:perimeter({circle,{0,0},1}).

perimeter_rectangle_test() ->
6 = assignment:perimeter({rectangle,{0,0},1,2}).

perimeter_triangle_test() ->
6 = assignment:perimeter({triangle,{0,0},1,2,3}).

% area tests
area_circle_test() ->
ExpectedCircleArea = math:pi(),
ExpectedCircleArea = assignment:area({circle,{0,0},1}).

area_rectangle_test() ->
2 = assignment:area({rectangle,{0,0},1,2}).

area_triangle_test() ->
% Testcase with 2 square angled
% triangles put together. I.e one side
% with 2, two with sqrt(2).
% Should have area = 1.
S = math:sqrt(2),
Actual = assignment:area({triangle,{0,0},2,S,S}),
between(Actual,0.9999,1.0001).


% enclose tests
enclose_circle_test() ->
{rectangle,{0,0},2,2} = assignment:enclose({circle,{0,0},1}).

enclose_rectangle_test() ->
Rect = {rectangle,{0,0},1,2},
Rect = assignment:enclose(Rect).

enclose_triangle_test() ->
S = math:sqrt(2),
Triangle = {triangle,{0,0},2,S,S},
{rectangle,{0,0},Height,2} = assignment:enclose(Triangle),
between(Height,0.9999,1.0001).

enclose_triangle2_test() ->
S = math:sqrt(2),
Triangle = {triangle,{0,0},S,2,S},
{rectangle,{0,0},Height,Width} = assignment:enclose(Triangle),
between(Height,S-0.0001,S+0.0001),
between(Width,S-0.0001,S+0.0001).

enclose_triangle3_test() ->
Triangle1 = {triangle,{0,0},3,4,6},
Triangle2 = {triangle,{0,0},6,4,3},
Triangle3 = {triangle,{0,0},4,3,6},
{rectangle,{0,0},Height1,Width1} = assignment:enclose(Triangle1),
{rectangle,{0,0},Height2,Width2} = assignment:enclose(Triangle2),
{rectangle,{0,0},Height3,Width3} = assignment:enclose(Triangle3),
between(Height1,Height2-0.0001,Height2+0.0001),
between(Width1,Width2-0.0001,Width2+0.0001),
between(Width1,Width3-0.0001,Width3+0.0001).


% bits tests
bits_test() ->
3 = assignment:bits(7),
1 = assignment:bits(8),
4 = assignment:bits(15),
1 = assignment:bits(16).

bitsT_test() ->
3 = assignment:bitsT(7),
1 = assignment:bitsT(8),
4 = assignment:bitsT(15),
1 = assignment:bitsT(16).
