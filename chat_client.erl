%%% File    : chat_client.erl
%%% Author  : Savas Aydin <savasaydin@gmail.com> ,
%%%           Aydan Halilov <a.halilov@student.gu.se>
%%% Description : This module is to do very basic client interaction functions.
%%                In our chat system usernames are key value.
%%% Created : 10 Dec 2010 by Savas Aydin <savasaydin@gmail.com>

-module(chat_client).

-compile(export_all).

%%------------------------------------------------------------------------------
%%connects and starts message_server module which is acting as router.
%%------------------------------------------------------------------------------
start()->
    message_server:start_link().

%%------------------------------------------------------------------------------
%%to register user name with specific password.
%%------------------------------------------------------------------------------
register_name(Name, Pass) ->
    message_server:register_name(Name,Pass).

%%------------------------------------------------------------------------------
%%after registeration user needs to log in to be able instant chat.
%%------------------------------------------------------------------------------
log_in(Name, Pass)->    
    Pid = spawn(chat_client, loop, [Name]),
    message_server:login_name(Name, Pass, Pid).

%%------------------------------------------------------------------------------
%%to log out from chat system.
%%------------------------------------------------------------------------------
log_out(Name)->
    message_server:log_out(Name).

%%------------------------------------------------------------------------------
%%to send message to specific name.
%%------------------------------------------------------------------------------
send_message(Name, MessageBody) ->
   message_server:send_message(Name, MessageBody).

%%------------------------------------------------------------------------------
%%receives print out message to print the messages to users
%%------------------------------------------------------------------------------
loop(Name) ->
    receive
	{print_msg, MessageBody} ->
	    io:format("~p received: ~p~n", [Name, MessageBody]),
	    loop(Name);
	stop ->
	    ok
    end.
