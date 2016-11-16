
-module(cast_reset_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([handle_get/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

% note that the method won't be called since the callback
% specified here will be only called when GET and HEAD request
% being processed.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, handle_get}
    ], Req, State}.


handle_get(Req, State) ->

    ok = cast_database:create_casts(),
    {<<"ok">>, Req, State}.
