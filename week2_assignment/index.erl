-module(index).
-export([get_file_contents/1,show_file_contents/1,
        get_words_in_string/1,
        uniq/1,
        remove/2,
        ranges/1,
        index_file/1,
        get_word_dict/1]).

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)
  

% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.
-spec get_file_contents([string()]) -> [string()].
get_file_contents(Name) ->
    {ok,File} = file:open(Name,[read]),
    Rev = get_all_lines(File,[]),
lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.

get_all_lines(File,Partial) ->
    case io:get_line(File,"") of
        eof -> file:close(File),
               Partial;
        Line -> {Strip,_} = lists:split(length(Line)-1,Line),
                get_all_lines(File,[Strip|Partial])
    end.

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.

-spec show_file_contents([string()]) -> atom().
show_file_contents([L|Ls]) ->
    io:format("~s~n",[L]),
    show_file_contents(Ls);
 show_file_contents([]) ->
    ok.    
     

% ###################################
% # My stuff follows here
% ###################################

% I made a first version with everything built on lists,
% but it was too slow to process the Dickens thing, albeit correct.
% So I rewrote, and instead of using a better structure
% only expressed with lists, I changed so it uses Erlangs dict type
% instead. I thought it would make more sence in a real world,
% and since we're reusing library functions anyway, why
% not do it realistically. All dictionary stuff is in the function get_word_dict.

% Fetch indexes for all extracted words in a file.
-spec index_file(string()) -> [{string(), [{integer(),integer()}]}].
index_file(Filename) ->
    Lines = get_file_contents(Filename),
    WordsInLines = lists:map(fun(L) -> uniq(get_words_in_string(L)) end, Lines),
    LineNumbersPerWord = get_word_dict(WordsInLines),
    RangesPerWord = lists:map(fun({W,Ns}) -> {W,ranges(Ns)} end, LineNumbersPerWord),
    lists:keysort(1,RangesPerWord).

% Create a dictonary, in list form, with mapping from words to linenumbers they occur in.
% I.e [{"word1",[1,2,3]},{"word2",[4,5,6]}].
% Uses two folds, one to "loop" over lines, and thus get linenumbers (the outer),
% and one to "loop" over the words in a line.
% Passes around a dictionary, with words as key and occurances as value.
-spec get_word_dict([[string()]]) -> [{string(),[integer()]}].
get_word_dict(WordsInLines) ->
    {_,WordDict} = lists:foldl(fun(Ws,{N,Dict}) -> % for each line
        NewDict = lists:foldl(fun(W,Dict2) -> % for each word
            dict:update(W, fun(Ls) -> [N|Ls] end, [N], Dict2) % store in dictionary
        end, Dict, Ws),
        {N+1, NewDict}
    end,{1,dict:new()},WordsInLines),
    Dict = dict:map(fun(_,Xs) -> lists:reverse(Xs) end,WordDict), % reverse the line numbers to get in proper, sorted order
    dict:to_list(Dict). % Convert to list when returning. Mostly for readability.

% Get all long enough words in a string as lowercase.
% Uses regular expressions to find the
% non word characters and split into parts accordingly. has the
% benefit of matching words correctly for other languages than English.
-spec get_words_in_string(string()) -> [string()].
get_words_in_string(S) ->
    {ok,REDivider} = re:compile("\\W+"), % \W in regex lingo is "non word characters".
    Words = re:split(S,REDivider,[{return,list}]),
    LongWords = lists:filter(fun(X) -> length(X) >= minimun_word_length() end, Words),
    lists:map(fun(W) -> string:to_lower(W) end, LongWords).

% Helper to store setting of when to filter out short words.
minimun_word_length() -> 4.

% Fetch ranges as pairs of {From,To} for a list of sorted integers.
-spec ranges([integer()]) -> {integer(),integer()}.
ranges([]) -> [];
ranges([N|Ns]) -> ranges(N,N+1,Ns).
ranges(Start,N,[]) -> [{Start,N-1}];
ranges(Start,N,[N|Ns]) -> ranges(Start,N+1,Ns);
ranges(Start,N1,[N2|Ns]) -> [{Start,N1-1} | ranges(N2,N2+1,Ns)].

% Filter so only unique entries in a list.
-spec uniq([T]) -> [T].
uniq([]) -> [];
uniq([X|Xs]) -> [X | uniq(remove(X,Xs))].

% Remove an entry from list
-spec remove(T,[T]) -> [T].
remove(_,[]) -> [];
remove(X,[X|Ys]) -> remove(X,Ys);
remove(X,[Y|Ys]) -> [Y|remove(X,Ys)].
