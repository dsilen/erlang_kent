% c(index), c(index_tests), eunit:test(index).

-module(index_tests).
-include_lib("eunit/include/eunit.hrl").

get_words_in_string_test() ->
?assertEqual([],index:get_words_in_string("")),
?assertEqual(["hello"],index:get_words_in_string("hello")),
?assertEqual(["hello"],index:get_words_in_string(" hello")),
?assertEqual(["hello"],index:get_words_in_string("hello ")),
?assertEqual(["hallå"],index:get_words_in_string("hallå")),
?assertEqual(["grüezi","mitenand"],index:get_words_in_string("Grüezi mitenand")).

uniq_test() ->
?assertEqual([],index:uniq([])),
?assertEqual([a],index:uniq([a,a])),
?assertEqual([a,b],index:uniq([a,a,b,b,a])).

get_word_dict_test() ->
W1 = "lorem", W2 = "ipsut", W3 = "dolorem",
?assertEqual([], index:get_word_dict([[]])),
?assertEqual([{W1,[1,2,3]},{W2,[1,2]},{W3,[3]}], index:get_word_dict([[W1,W2],[W2,W1],[W1,W3]])).

ranges_test() ->
?assertEqual([{1,3},{5,6},{9,9}],index:ranges([1,2,3,5,6,9])),
?assertEqual([{9,9}],index:ranges([9])),
?assertEqual([],index:ranges([])).
