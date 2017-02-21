% The assignment is tested in the file
% assignment_tests.erl. Someone put a comment on how
% to use eunit, and I like unit tests... :)

-module(assignment).
-export([
    perimeter/1,
    area/1,
    enclose/1,
    bits/1,
    bitsT/1]).


% Assuming the shapes are as defined
% in the videos, i.e
% {circle,{X,Y},R} where {X,Y} defines the centre and R is the radius.
% {rectangle,{X,Y},H,W} ...
% defining triangle similarly as
% {triangle,{X,Y},Side1,Side2,Side3}

% Perimeter:
perimeter({circle,_,Radius}) ->
    2 * math:pi() * Radius;

perimeter({rectangle,_,Height,Width}) ->
    2 * Width + 2 * Height;

perimeter({triangle,_,Side1,Side2,Side3}) ->
    Side1 + Side2 + Side3.



% Area:
area({circle,_,Radius}) ->
    math:pi() * Radius * Radius;

area({rectangle,_,Height,Width}) ->
    Height * Width;

area({triangle,_,A,B,C}) ->
% calculating using Heron's formula
% https://en.wikipedia.org/wiki/Heron's_formula
    S = (A + B + C) / 2, % could use the perimeter function here.
    math:sqrt(S * (S-A) * (S-B) * (S-C)).



% Enclose:
enclose({circle,Where,Radius}) ->
    {rectangle,Where,Radius*2,Radius*2};

enclose({rectangle,Where,Height,Width}) -> % could use a guard and check
    {rectangle,Where,Height,Width}; % if rectangle and then just be identity function.

enclose({triangle,Where,A,B,C}) ->
    Area = area({triangle,Where,A,B,C}),
    Height = Area * 2 / A,
    % The area of the rectangle is the same
    % regardless if looking from A, B or C's perspective.
    {rectangle,Where,Height,A}.



% Bits direct
bits(0) -> 0;
bits(N) when N > 0 ->
    bits(N div 2) + N rem 2.

% Bits tail recursive
bitsT(N) when N > 0 -> bitsT(N,0).
bitsT(0,Acc) -> Acc;
bitsT(N,Acc) -> bitsT(N div 2,Acc + N rem 2).

% Which bits-implementation I think is better:
% direct is more readable but can get stack overflow.
% tail recursive can't. In this case no big difference,
% I would probably write the direct one.
