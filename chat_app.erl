%%% File    : chat_app.erl
%%% Author  : Savas Aydin <savasaydin@gmail.com>
%%% Description :This module is to start and stop application as long as chat.app is loaded to applications. 
%%% Created : 16 Dec 2010 by Savas Aydin <savasaydin@gmail.com>

-module(chat_app).

-behaviour(application).

-export([start/2,stop/1]).

%%------------------------------------------------------------------------------
%% This function starts application using application:start/1 or 2.
%%------------------------------------------------------------------------------
start(_,A)->
    chat_supervisor:start_link().

%%------------------------------------------------------------------------------
%%this stops the application. Return value is ignored -> void().
%%------------------------------------------------------------------------------
stop(_state)->
    ok.
