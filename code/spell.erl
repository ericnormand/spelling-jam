-module(spell).
-compile(export_all).
-import(lists, [reverse/1]).

%% Comment real time updating costs $10,000
%%  Proposal

loop(D) ->
    receive
	{From, is_word, W} ->
	    From ! {self(), dict:find(W, D)},
	    loop(D);
	{update_dict, W} ->
	    D1 = case dict:find(W, D) of
		     {ok, N} -> dict:store(W, N+1, D);
		     error   -> dict:store(W, 1, D)
		 end,
	    loop(D1)
    end.

is_word(Pid, W) ->
    Pid ! {self(), is_word, W},
    receive
	{Pid, Reply} ->
	    case Reply of
		error -> false;
		{ok, _}  -> true
	    end
    end.

test() ->
    edits1("freddy").

alphabet() ->
    "abcdefghijklmnopqrstuvwxyz".

edits1(Word) ->
    Splits = splits(Word, [], []),
    %% io:format("Splits:~p~n",[Splits]),
    Deletes = [A++tl(B) || {A,B} <- tl(Splits), B =/= []],
    %% io:format("Delets:~p~n",[Deletes]),
    Transposes = [A++[B1,B0|BT] || {A,[B0,B1|BT]} <- Splits],
    Replaces = [A ++ [C|BT] || {A,[_|BT]} <- Splits,
			       C <- alphabet()],
    Inserts = [A ++ [C|B] || {A,B} <- Splits,
			     C <- alphabet()],
    Deletes ++ Transposes ++ Replaces ++ Inserts.

%% This is the example use
top() ->
    Pid = spawn(fun() ->
			S = read_words(),
			loop(S)
		end),
    L = known_edits2("edlipses", Pid),
    L1 = lists:sort([{is_word(Pid, list_to_binary(I)), I} || I <- L]),
    L2 = remove_dups(L1).

remove_dups([H,H|T]) -> remove_dups([H|T]);
remove_dups([H|T])   -> [H|remove_dups(T)];
remove_dups([])      -> [].

known_edits2(Word, Pid) ->
    [E2 || E1 <- edits1(Word),
	   E2 <- edits1(E1),
	   is_word(Pid, list_to_binary(E2))].

%% Compute word set and store on disk
%%  Note nice use of term_to_binary
%%  and inverse binary_to_term in read_words

train() ->
    {ok, Bin} = file:read_file("../data/big.txt"),
    L = binary_to_list(Bin),
    L1 = string:to_lower(L),
    L2 = re:split(L1,"[^a-z]+"),
    D = count_frequencies(L2),
    file:write_file("words.set", [term_to_binary(D)]).

read_words() ->
    case file:read_file("words.set") of
        {ok, B}         -> binary_to_term(B);
        {error, enoent} -> train(),
                           {ok, B} = file:read_file("words.set"),
                           binary_to_term(B)
    end.

%% Make a dictionary of Word -> Count

count_frequencies(L)->
    count_frequencies(L, dict:new()).

count_frequencies([H|T], D) ->
    D1 = case dict:find(H, D) of
	     {ok, N} -> dict:store(H, N+1, D);
	     error   -> dict:store(H, 1, D)
	 end,
    count_frequencies(T, D1);
count_frequencies([], D) ->
    D.

splits([H|T], L1, L) -> splits(T, [H|L1], [{reverse(L1), [H|T]}|L]);
splits([], L1, L)    -> [{reverse(L1),[]}|L].
