-module(t_server).
-import(t_client, [initUser/3, beginSim/3]).
-import(lists, [nth/2, append/1]).
-export([
    start/2,
    genActors/4,
    genList/2,
    genNewList/3,
    simulator/4,
    makeOnline/2,
    replacenth/3,
    switchOffline/2
]).
-record(user, {
    id, subscribers = [], subscribing = [], tweet = [], feed = [], mentions = [], status = false
}).

start(NumUsers, NumTweets) ->
    io:format("Number of Users : ~p ~n Number of Tweets : ~p ~n", [NumUsers, NumTweets]),
    % Actors=genActors(1,NumUsers,self()),
    Actors = gen(1, NumUsers, self()),
    io:format("Users Registered for following Actors : ~n ~p ~n", [Actors]),
    Rec_List = genList(1, NumUsers),
    HTs = #{"ht1" => [], "ht2" => [], "ht3" => [], "ht4" => [], "ht5" => []},
    spawn(t_client, beginSim, [Actors, self(), 1]),
    simulator(Rec_List, HTs, NumTweets, 0).

gen(S, E, PPid) ->
    genActors(S, E, [], PPid).


postMention(List, Uid, Tweet) ->
    P = nth(Uid, List),
    MentionList = P#user.mentions,
    P1 = P#user{mentions = lists:append(MentionList, [Tweet])},
    replacenth(List, Uid, P1).

reTweeting(Uid, List, RandTweet) ->
    P = nth(Uid, List),
    TweetList = P#user.tweet,
    io:format("RETWEETED : ~p ~n", [RandTweet]),
    P1 = P#user{tweet = lists:append(TweetList, [RandTweet])},
    replacenth(List, Uid, P1).

addNewSubscribing(Uid, SubscribeUid, List) ->
    P = nth(SubscribeUid, List),
    SubscribingList = P#user.subscribing,
    P1 = P#user{subscribing = lists:append(SubscribingList, [Uid])},
    replacenth(List, SubscribeUid, P1).

addNewSubscriber(Uid, SubscribeUid, List) ->
    P = nth(Uid, List),
    SubscriberList = P#user.subscribers,
    P1 = P#user{subscribers = lists:append(SubscriberList, [SubscribeUid])},
    replacenth(List, Uid, P1).

displayFeed(Uid, List) ->
    P = nth(Uid, List),
    P#user.feed.

displayM(Uid, List) ->
    P = nth(Uid, List),
    P#user.mentions.

distributeTweet(List, Uid, Tweet, SubscriberList, S, Length) ->
    case S =< Length of
        true ->
            Curr = nth(S, SubscriberList),
            P = nth(Curr, List),
            FeedList = P#user.feed,
            P1 = P#user{feed = lists:append(FeedList, [Tweet])},
            List2 = replacenth(List, Curr, P1),
            distributeTweet(List2, Uid, Tweet, SubscriberList, S + 1, Length);
        false ->
            List
    end.

postTweet(List, Uid, Tweet) ->
    P = nth(Uid, List),
    TweetList = P#user.tweet,
    P1 = P#user{tweet = lists:append(TweetList, [Tweet])},
    replacenth(List, Uid, P1).

switchOffline(List, Uid) ->
    P = lists:nth(Uid, List),
    Bool = P#user.status,
    case Bool == true of
        true ->
            P1 = P#user{status = false},
            replacenth(List, Uid, P1);
        false ->
            io:format("A - OFFLINE : User Already Offline~n", []),
            List
    end.

switchOnline(List, Uid) ->
    P = lists:nth(Uid, List),
    Bool = P#user.status,
    case Bool == false of
        true ->
            P1 = P#user{status = true},
            replacenth(List, Uid, P1);
        false ->
            io:format("A - ONLINE : User Already Online~n", []),
            List
    end.

addToSubscribing(Uid, SubscriberList, List, S, Length) ->
    case S =< Length of
        true ->
            Curr = nth(S, SubscriberList),
            P = nth(Curr, List),
            TempList = P#user.subscribing,
            P1 = P#user{subscribing = lists:append(TempList, [Uid])},
            List2 = replacenth(List, Curr, P1),
            addToSubscribing(Uid, SubscriberList, List2, S + 1, Length);
        false ->
            List
    end.

addSubscribers(Uid, SubscriberList, List) ->
    P = nth(Uid, List),
    P1 = P#user{subscribers = SubscriberList},
    replacenth(List, Uid, P1).

makeOnline(List, Uid) ->
    P = nth(Uid, List),
    P1 = P#user{status = true},
    replacenth(List, Uid, P1).

replacenth(L, Index, NewValue) ->
    {L1, [_ | L2]} = lists:split(Index - 1, L),
    L1 ++ [NewValue | L2].

%E is numNodes
genActors(S, E, L, PPid) ->
    case S =< E of
        true ->
            genActors(
                S + 1, E, lists:append([L, [spawn(t_client, initUser, [S, PPid, E])]]), PPid
            );
        false ->
            io:format("GENERATE : Users Generated~n"),
            L
    end.

genList(S, E) ->
    genNewList(S, E, []).

genNewList(S, E, L) ->
    case S =< E of
        true ->
            genNewList(
                S + 1,
                E,
                lists:append([
                    L,
                    [
                        #user{
                            id = S,
                            subscribers = [],
                            subscribing = [],
                            tweet = [],
                            feed = [],
                            mentions = [],
                            status = false
                        }
                    ]
                ])
            );
        false ->
            L
    end.

simulator(List, HTs, NumTweets, TotTweets) ->
    receive
        {addSubscribers, SubscribeUid, Uid} ->
            P = nth(Uid, List),
            SubscriberList = P#user.subscribers,
            VerifyMember = lists:member(SubscribeUid, SubscriberList),
            case VerifyMember == false of
                true ->
                    io:format("SUBSCRIBE : User ~p is following ~p ~n", [SubscribeUid,Uid]),
                    List1 = addNewSubscriber(Uid, SubscribeUid, List),
                    List2 = addNewSubscribing(Uid, SubscribeUid, List1),
                    simulator(List2, HTs, NumTweets, TotTweets);
                false ->
                    simulator(List, HTs, NumTweets, TotTweets)
            end;
        {makeOnline, Uid, Pid} ->
            io:format("ONLINE : User ~p is online (PID : ~p) ~n", [Uid, Pid]),
            List1 = makeOnline(List, Uid),
            simulator(List1, HTs, NumTweets, TotTweets);
        {setFollowers, Uid, SubscriberList} ->
            io:format("SUBSCRIBERS : Subscribers ~p assigned to ~p ~n", [SubscriberList, Uid]),
            List1 = addSubscribers(Uid, SubscriberList, List),
            Length = length(SubscriberList),
            List2 = addToSubscribing(Uid, SubscriberList, List1, 1, Length),
            simulator(List2, HTs, NumTweets, TotTweets);
        {switch_off, Uid} ->
            List1 = switchOffline(List, Uid),
            io:format("OFFLINE : User ~p is now offline~n", [Uid]),
            simulator(List1, HTs, NumTweets, TotTweets);
        {switch_on, Uid} ->
            List1 = switchOnline(List, Uid),
            io:format("ONLINE : User ~p is now online~n", [Uid]),
            simulator(List1, HTs, NumTweets, TotTweets);
        {postTweet, Uid, Tweet} ->
            List1 = postTweet(List, Uid, Tweet),
            P = nth(Uid, List1),
            SubscriberList = P#user.subscribers,
            Length = length(SubscriberList),
            SumTotTweets=Length+TotTweets,
            List2 = distributeTweet(List1, Uid, Tweet, SubscriberList, 1, Length),
            ContainsHT = string:chr(Tweet, $#),
            ContainsM = string:chr(Tweet, $@),
            FirstSpace = string:chr(Tweet, $\s ),
            case SumTotTweets<NumTweets of
                true->
                    case ContainsHT =/= 0 of
                        true ->
                            Start = ContainsHT + 1,
                            End = ContainsHT + 4,
                            Tag = string:substr(Tweet, Start, End),
                            TagL = maps:get(Tag, HTs),
                            TagL2 = lists:append(TagL, [Tweet]),
                            Map2 = maps:put(Tag, TagL2, HTs),
                            case ContainsM =/= 0 of
                                true ->
                                    Start1 = ContainsM + 1,
                                    MentionedUser = string:substr(Tweet, Start1, FirstSpace - 1),
                                    UserMentioned = integer_to_list(MentionedUser),
                                    List3 = postMention(List2, UserMentioned, Tweet),
                                    simulator(List3, Map2, NumTweets, SumTotTweets);
                                false ->
                                    simulator(List2, Map2, NumTweets, SumTotTweets)
                            end;
                        false ->
                            case ContainsM =/= 0 of
                                true ->
                                    Start2 = ContainsM + 1,
                                    MentionedUser = string:substr(Tweet, Start2, FirstSpace),
                                    UserMentioned = integer_to_list(MentionedUser),
                                    List3 = postMention(List2, UserMentioned, Tweet),
                                    simulator(List3, HTs, NumTweets, SumTotTweets);
                                false ->
                                    simulator(List2, HTs, NumTweets, SumTotTweets)
                            end 
                    end;
                false->
                    io:format("TERMINATING: Post tweets max limit ~p reached ~n", [NumTweets])
            end;

        {display_hashtags, HT} ->
            TweetList = maps:get(HT, HTs),
            io:format("HASHTAGS : Tweets with ~p hashtags: ~p ~n", [HT, TweetList]),
            simulator(List, HTs, NumTweets, TotTweets);
        {display_mentions, Uid} ->
            MentionList = displayM(Uid, List),
            io:format("MENTIONS : User mentions for user ~p are: ~p ~n", [Uid, MentionList]),
            simulator(List, HTs, NumTweets, TotTweets);
        {get_feed, Uid} ->
            Feed = displayFeed(Uid, List),
            io:format("GET FEED : Displaying feed for ~p user: ~p~n", [Uid, Feed]),
            simulator(List, HTs, NumTweets, TotTweets);
        {retweet, Uid} ->
            P = nth(Uid, List),
            FeedList = P#user.feed,
            FeedListLength = length(FeedList),
            SubscriberList = P#user.subscribers,
            Length = length(SubscriberList),
            SumTotTweets=Length+TotTweets,
            case SumTotTweets<NumTweets of
                true->
                    io:format("RETWEET : User ~p is retweeting~n", [Uid]),
                    case FeedListLength =/= 0 of
                        true ->
                            RandTweetIdx = rand:uniform(FeedListLength),
                            RandTweet = nth(RandTweetIdx, FeedList),
                            List1 = reTweeting(Uid, List, RandTweet),
                            io:format("RETWEET2 : User ~p is retweeting~n", [RandTweet]),
                            List2 = distributeTweet(List1, Uid, RandTweet, SubscriberList, 1, Length),
                            ContainsHT = string:chr(RandTweet, $#),
                            case ContainsHT =/= 0 of
                                true ->
                                    Start = ContainsHT + 1,
                                    End = ContainsHT + 4,
                                    Tag = string:substr(RandTweet, Start, End),
                                    TagL = maps:get(Tag, HTs),
                                    TagL2 = lists:append(TagL, [RandTweet]),
                                    Map2 = maps:put(Tag, TagL2, HTs),
                                    simulator(List2, Map2, NumTweets, SumTotTweets);
                                false ->
                                    simulator(List2, HTs, NumTweets, SumTotTweets)
                            end;
                        false ->
                            io:format("Cannot retweet; no tweets in feed ~n", []),
                            simulator(List, HTs, NumTweets, SumTotTweets)
                    end;
                false->
                    io:format("TERMINATING : Max limit ~p tweets reached ~n", [NumTweets])
            end
    end.

