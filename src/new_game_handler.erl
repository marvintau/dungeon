%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(new_game_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([hello_to_html/2]).
-export([hello_to_json/2]).
-export([hello_to_text/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {[
        {<<"text/html">>, hello_to_html},
        {<<"application/json">>, hello_to_json},
        {<<"text/plain">>, hello_to_text}
    ], Req, State}.

hello_to_html(Req, State) ->
    
    {done, Result} = battle:init_new_battle(json),

    Body = Result,
    {Body, Req, State}.

hello_to_json(Req, State) ->

    erlang:display(we_got_json),

    {done, Result} = battle:init_new_battle(json),

    Body = Result,
    {Body, Req, State}.

hello_to_text(Req, State) ->
    {<<"REST Hello World as text!">>, Req, State}.
