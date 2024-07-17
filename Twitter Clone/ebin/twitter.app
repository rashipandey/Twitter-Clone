{application, 'twitter', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['client','feed_handl','helper','register_handl','retweet_handl','search_hashtag_handl','search_mention_handl','search_subscribe_handl','subscribe_handl','tweet_handl','twitter','twitter_app','twitter_supvis']},
	{registered, [twitter_sup]},
	{applications, [kernel,stdlib,cowboy]},
	{mod, {twitter_app, []}},
	{env, []}
]}.