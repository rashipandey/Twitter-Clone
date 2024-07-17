-module(twitter_app).

-behaviour(application).

-import(twitter, [get_server/0]).
-import(client, []).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Id = twitter:get_server(),
    io:fwrite("jfdssddsf"),
    {ok, Fd} = file:open("server.txt", [write]),
    file:write(Fd, pid_to_list(Id)),
    file:close(Fd),
    Dispatch =
        cowboy_router:compile([{'_',
                                [{"/",
                                  cowboy_static,
                                  {file,
                                   "/home/kali/Desktop/Twitter-Clone-main/twitter/static/register.html"}},
                                 {"/register", register_handl, []},
                                 {"/:name/main",
                                  cowboy_static,
                                  {file,
                                   "/home/kali/Desktop/Twitter-Clone-main/twitter/static/main.html"}},
                                 {"/:name/tweet", tweet_handl, []},
                                 {"/:name//search/hashtag", search_hashtag_handl, []},
                                 {"/:name/search/mention", search_mention_handl, []},
                                 {"/:name/search/subscribe", search_subscribe_handl, []},
                                 {"/:name/subscribe", subscribe_handl, []},
                                 {"/:name/feed", feed_handl, []},
                                 {"/:name/retweet", retweet_handl, []}]}]),
    {ok, _} =
        cowboy:start_clear(my_http_listener, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
    twitter_supvis:start_link().

stop(_State) ->
    ok.
