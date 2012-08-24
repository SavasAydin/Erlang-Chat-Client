%%% File    : chat_supervisor.erl
%%% Author  : Savas Aydin <savasaydin@gmail.com>
%%% Description : This supervisor behaviour to start individual worker processes
%%                in supervision tree.
%%% Created : 16 Dec 2010 by Savas Aydin <savasaydin@gmail.com>>

-module(chat_supervisor).

-behaviour(supervisor).

-define(SERVER, ?MODULE).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%------------------------------------------------------------------------------
%% This function initiliaze the supervisor strategy. If any process crashes, 
%% restart all the process depends on supervisor. If any process crashes 5 times
%% in 30 seconds and then terminate the all the processes.
%%------------------------------------------------------------------------------
init([]) ->
    io:format("~p init~n",[?MODULE]),
  Message_server = {message_server, {message_server, start_link, []},
		   permanent, 5000, worker, [message_server]},
  DB_server =      {db_server, {db_server, start_link, []},
		   permanent, 5000, worker, [db_server]},
  {ok, {{one_for_all, 5, 30}, [Message_server, DB_server ]}}.
