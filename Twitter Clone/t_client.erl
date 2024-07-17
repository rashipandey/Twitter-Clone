-module(t_client).
-import(t_server, [start/2]).
-export([initUser/3, beginSim/3]).

initUser(Uid, PPid, NumUsers) ->
    PPid ! {makeOnline, Uid, self()},
    assignSubscribersZipf(Uid, PPid, NumUsers).

% initUser(Uid, PPid, NumUsers) ->
%     PPid ! {makeOnline, Uid, self()},
%     assignSubscribers(Uid, PPid, NumUsers).

assignSubscribers(Uid, PPid, NumUsers) ->
    NumFollowers = rand:uniform(NumUsers - 2) + 1,
    List = [rand:uniform(NumUsers) || _ <- lists:seq(1, NumFollowers)],
    List2 = checkList(List, Uid),
    Set = ordsets:from_list(List2),
    FollowerList = ordsets:to_list(Set),
    PPid ! {setFollowers, Uid, FollowerList}.

assignSubscribersZipf(Uid, PPid, NumUsers) ->
    Divisor=Uid+1,
    NumFollowers = NumUsers div Divisor,
    io:format("Number of followers for Uid ~p are: ~p ~n", [Uid,NumFollowers]),
    List = [rand:uniform(NumUsers) || _ <- lists:seq(1, NumFollowers)],
    List2 = checkList(List, Uid),
    Set = ordsets:from_list(List2),
    FollowerList = ordsets:to_list(Set),
    PPid ! {setFollowers, Uid, FollowerList}.

beginSim(Actors, PPid, S) ->
    Op_List = [
        "tweet",
        "switch_off",
        "switch_on",
        "follow",
        "query_hashtags",
        "query_mentions",
        "get_feed",
        "retweet"
    ],
    HTList = ["ht1", "ht2", "ht3", "ht4", "ht5"],
    case S =< 20 of
        true ->
            NumUsers = length(Actors),
            RandUid = rand:uniform(NumUsers),
            Op_Index = rand:uniform(length(Op_List)),
            Op = lists:nth(Op_Index, Op_List),
            io:format("Op selected : ~p ~n", [Op]),
            case Op == "tweet" of
                true ->
                    io:format("TWEET by : ~p (Uid) ~n", [RandUid]),
                    switchOnline(PPid, RandUid),
                    makeTweet(PPid, RandUid, NumUsers);
                false ->
                    case Op == "switch_off" of
                        true ->
                            switchOffline(PPid, RandUid);
                        false ->
                            case Op == "switch_on" of
                                true ->
                                    switchOnline(PPid, RandUid);
                                false ->
                                    case Op == "follow" of
                                        true ->
                                            UidToSubscribe = rand:uniform(NumUsers),
                                            subscribeUser(NumUsers, UidToSubscribe, RandUid, PPid);
                                        false ->
                                            case Op == "query_hashtags" of
                                                true ->
                                                    SelectHT = rand:uniform(5),
                                                    HT = lists:nth(SelectHT, HTList),
                                                    queryHTs(HT, PPid);
                                                false ->
                                                    case Op == "query_mentions" of
                                                        true ->
                                                            queryMs(RandUid, PPid);
                                                        false ->
                                                            case Op == "get_feed" of
                                                                true ->
                                                                    queryFeed(RandUid, PPid);
                                                                false ->
                                                                    case Op == "retweet" of
                                                                        true ->
                                                                            reTweet(
                                                                                RandUid, PPid
                                                                            );
                                                                        false ->
                                                                            ""
                                                                    end
                                                            end
                                                    end
                                            end
                                    end
                            end
                    end
            end,
            beginSim(Actors, PPid, S + 1);
        false ->
            ""
    end.

reTweet(Uid, PPid) ->
    PPid ! {retweet, Uid}.

subscribeUser(NumUsers, SubscribeUid, Uid, PPid) ->
    case SubscribeUid == Uid of
        true ->
            NewUidToSub = rand:uniform(NumUsers),
            subscribeUser(NumUsers, NewUidToSub, Uid, PPid);
        false ->
            PPid ! {addSubscribers, SubscribeUid, Uid}
    end.

queryFeed(Uid, PPid) ->
    PPid ! {get_feed, Uid}.

queryMs(Uid, PPid) ->
    PPid ! {display_mentions, Uid}.

queryHTs(HT, PPid) ->
    PPid ! {display_hashtags, HT}.

makeTweet(PPid, Uid, NumUsers) ->
    TweetHT = genRandTweets(),
    TweetM = genTweetM(Uid, NumUsers, TweetHT),
    io:format("TWEET by : ~p (Uid) as : ~p ~n", [Uid,TweetM]),
    PPid ! {postTweet, Uid, TweetM}.

genTweetM(Uid, NumUsers, TweetHT) ->
    SelectM = rand:uniform(2),
    case SelectM == 1 of
        true ->
            User = genRandUser(Uid, NumUsers),
            UserToList = integer_to_list(User),
            Mention2 = string:concat("@", UserToList),
            TempMention = string:concat(Mention2, " "),
            StrNew = string:concat(TempMention, TweetHT),
            StrNew;
        false ->
            TweetHT
    end.

genRandUser(Uid, NumUsers) ->
    SelectRandUser = rand:uniform(NumUsers),
    case SelectRandUser == Uid of
        true ->
            genRandUser(Uid, NumUsers);
        false ->
            SelectRandUser
    end.

genRandTweets() ->
    WordList = ["Not", "Lot", "Of", "Things", "Worth", "Caring", "About"],
    HTList = ["ht1", "ht2", "ht3", "ht4", "ht5"],
    ChooseHT = rand:uniform(2),
    NumWords = rand:uniform(7),
    List = [rand:uniform(NumWords) || _ <- lists:seq(1, NumWords)],
    Length = length(List),
    String = genTweet(WordList, "", 1, Length, List),
    case ChooseHT == 1 of
        true ->
            SelectHT = rand:uniform(5),
            HT = lists:nth(SelectHT, HTList),
            HashTag = string:concat("#", HT),
            StringHT = string:concat(String, HashTag),
            StringHT;
        false ->
            String
    end.

genTweet(WordList, String, S, Length, List) ->
    case S =< Length of
        true ->
            Curr = lists:nth(S, List),
            Str1 = lists:nth(Curr, WordList),
            Str2 = string:concat(String, Str1),
            Str3 = string:concat(Str2, " "),
            genTweet(WordList, Str3, S + 1, Length, List);
        false ->
            String
    end.

switchOffline(PPid, Uid) ->
    PPid ! {switch_off, Uid}.

switchOnline(PPid, Uid) ->
    PPid ! {switch_on, Uid}.

checkList(List, Uid) ->
    Bool = lists:member(Uid, List),
    case Bool of
        true ->
            lists:delete(Uid, List);
        false ->
            List
    end.
