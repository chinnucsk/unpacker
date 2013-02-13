-module(media).

-export([guess/1]).

guess(FileName) ->
	{ok, Application} = application:get_application(?MODULE),
	Script = filename:join(code:priv_dir(Application), "guessit_erl_bridge.py"),
	Port = open_port({spawn, Script},
        [{packet, 1}, binary, use_stdio]),
    port_command(Port, term_to_binary({guess, FileName})),
    receive
        {Port, {data, RespData}} ->
            {ok, binary_to_term(RespData)}
    after
        5000 ->
            {error, timeout}
    end.