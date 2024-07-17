-module(search_hashtag_handl).

-import(helper, [get_id/0]).

-behavior(cowboy_handler).

-export([init/2]).

get_answers(S, []) ->
    S;
get_answers(S, L) ->
    [H | T] = L,
    get_answers(S ++ H ++ "\n", T).

init(Req0, State) ->
    Id = get_id(),
    Qs = cowboy_req:parse_qs(Req0),
    {_, Hashtag} = lists:keyfind(<<"hashtag">>, 1, Qs),
    Username = binary_to_list(cowboy_req:binding(name, Req0)),
    A = client:search_hashtag(Username, binary_to_list(Hashtag), Id),
    B = get_answers("", A),
    io:format("~p ~n", [A]),
    Req = cowboy_req:reply(200,
                           #{<<"content-type">> => <<"text/html">>},
                           ["<html>\n    <head>\n <style>\n\th1 {\n\t\tcolor: blue; \n\t "
                            "  font-family: 'Trebuchet MS', 'Lucida Sans Unicode', 'Lucida "
                            "Grande', 'Lucida Sans', Arial, sans-serif;\n\t}\n\th2{\n\t\tcolor: "
                            "royalblue; \n\t   font-family: 'Trebuchet MS', 'Lucida Sans "
                            "Unicode', 'Lucida Grande', 'Lucida Sans', Arial, sans-serif;\n\t}\n\t"
                            ".column {\n\tfloat: left;\n\twidth: 60px;\n\tpadding: 5px;\n\theight"
                            ": 45px;\n\t}\n\t.row::after {\n\tcontent: ;\n\tclear: both;\n\tdispl"
                            "ay: table;\n\t}\n\t#name {\n\tborder-radius: 10px;\n\tbackground: "
                            "#ffffff;\n\tpadding: 10px;\n\tborder-color: #36CFFF;\n\t}\n\t#passwo"
                            "rd {\n\tborder-radius: 10px;\n\tbackground: #ffffff;\n\tpadding: "
                            "10px;\n\tborder-color: #36CFFF;\n\t}\n\tp.text1 {\n\tfont-family: "
                            "'Trebuchet MS', 'Lucida Sans Unicode', 'Lucida Grande', 'Lucida "
                            "Sans', Arial, sans-serif;\n\tfont-weight: bold;\n\tcolor: royalblue; "
                            "\n\t}\n\tp.text2 {\n\tfont-family: 'Trebuchet MS', 'Lucida "
                            "Sans Unicode', 'Lucida Grande', 'Lucida Sans', Arial, sans-serif;\n\t"
                            "font-weight: bold;\n\tcolor: rgb(36, 131, 255); \n\t}\n\tinput[type="
                            "button], input[type=reset], input[type=submit] {\n\tbackground-color"
                            ": #1fb7cf;\n\tborder-radius: 10px;\n\tborder: none;\n\tcolor: "
                            "white;\n\tpadding: 8px 32px;\n\ttext-decoration: none;\n\tfont-famil"
                            "y: 'Trebuchet MS', 'Lucida Sans Unicode', 'Lucida Grande', "
                            "'Lucida Sans', Arial, sans-serif;\n\tmargin: 4px 2px;\n\tcursor: "
                            "pointer;\n\t}\n</style>        <title>twitter</title>\n    "
                            "</head>\n<body><p>",
                            B,
                            "</p></body"],
                           Req0),

    io:fwrite("~p ~n", [A]),
    {ok, Req, State}.
