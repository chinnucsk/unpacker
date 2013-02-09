%%%-------------------------------------------------------------------
%%% @author Stefan Hagdahl <stefan@scriptersserver.bredbandsbolaget.se>
%%% @copyright (C) 2013, Stefan Hagdahl
%%% @doc
%%%
%%% @end
%%% Created :  9 Feb 2013 by Stefan Hagdahl <stefan@scriptersserver.bredbandsbolaget.se>
%%%-------------------------------------------------------------------
-module(unpacker).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([add_to_queue/1]).
-export([list/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([exec_complete/1]).

-define(COMPLETE, filename:join(code:priv_dir(unpacker), "complete.py")).
-define(TIMEOUT, infinity).

-record(torrent, {
          path :: string()
         }).

-record(state, {
          queue :: [#torrent{}],
          port   :: port()
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_to_queue(Torrent) ->
    gen_server:cast(?MODULE, {add, Torrent}).

list() ->
    gen_server:call(?MODULE, list, ?TIMEOUT).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{queue=[]}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(list, _From, #state{queue=Queue}=State) ->
    {reply, Queue, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({add, Torrent}, #state{queue=Queue, port=undefined}=State) ->
    %% TODO: log action
    {Port, NewQueue} = process_queue(Queue ++ [#torrent{path=Torrent}]),
    io:format("Processed queue~nNewQueue:~p~n", [NewQueue]),
    {noreply, State#state{queue=NewQueue, port=Port}};
handle_cast({add, Torrent}, #state{queue=Queue}=State) ->
    %% TODO: log action
    io:format("Added Torrent(~p) to queue~n", [Torrent]),
    {noreply, State#state{queue=Queue ++ [#torrent{path=Torrent}]}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({Port, {exit_status, ExitCode}}, #state{queue=Queue, port=Port}=State) ->
    %% TODO: log exit
    io:format("Complete script exited with ExitCode:~p~n", [ExitCode]),
    {NewPort, NewQueue} = process_queue(Queue),
    {noreply, State#state{queue=NewQueue, port=NewPort}};
handle_info(Info, State) ->
    io:format("Info:~p~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
process_queue([]) ->
    {undefined, []};
process_queue([Torrent | Rest]) ->
    io:format("Torrent:~p~n", [Torrent]),
    Port = exec_complete(Torrent#torrent.path),
    {Port, Rest}.

exec_complete(Torrent) ->
    Cmd = lists:concat([?COMPLETE, " ", Torrent]),
    erlang:open_port({spawn, Cmd}, [exit_status]).
