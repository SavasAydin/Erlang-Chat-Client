%%% File    : message_server.erl
%%% Author  : Savas Aydin <savasaydin@gmail.com>
%%% Description :This module is router in chat system.   
%%% Created : 10 Dec 2010 by Savas <dartagnan@Dartagnan>
%%% References : http://www.freepatentsonline.com/6499053.html
%%%              http://weblog.hypotheticalabs.com/ 
%%%                                    author :Kevin Smith 
%%%                                 -  article: Erlang Pattern-> The Router
%%%                                 -  book : Erlang in practice 

-module(message_server).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API Client
-export([start_link/0,register_name/2,login_name/3,log_out/1,send_message/2, shutdown/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

%%------------------------------------------------------------------------------
%% Client functions
%%------------------------------------------------------------------------------
%%% Functions here to call for registeration, logging in, logging out, sending message and shutting down the router.
%%------------------------------------------------------------------------------
start_link() ->
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

register_name(UserName, Password) ->
  gen_server:call({global,?SERVER}, {register_name, UserName, Password}).

login_name(UserName, Password, UserPid) ->
    gen_server:call({global,?SERVER}, {log_in, UserName, Password, UserPid}).

log_out(UserName)->
    gen_server:call({global,?SERVER}, {log_out, UserName}).

send_message(UserName, Message) ->
  gen_server:call({global,?SERVER}, {send_msg, UserName, Message}).

shutdown() ->
  gen_server:cast({global,?SERVER}, stop).

%%------------------------------------------------------------------------------
%% gen_server callback functions
%%trap exit is to inform supervisor about crashes.
%%------------------------------------------------------------------------------
init([]) ->
  process_flag(trap_exit,true),
  io:format("~p ~p starting..~n",[?MODULE,self()]),
%  db_server:start_link(),
  {ok, #state{}}.

%%------------------------------------------------------------------------------
%%write the username and password into table in mnesia by using db_server 
%%functions as long as it receives the register_name message.
%%------------------------------------------------------------------------------
handle_call({register_name, UserName,Password}, _From, State) ->
  db_server:regist(UserName, Password),
  {reply, ok, State};

%%------------------------------------------------------------------------------
%It allows user to log_in and as soon as user logs in, 
%%user will get the messages that stored in db for him/her.
%%------------------------------------------------------------------------------
handle_call({log_in, UserName, Password, UserPid}, _From, State)->
  case db_server:login(UserName, Password) of
      logged_in->
	  Stored_Messages = db_server:find_messages(UserName),
	  lists:foreach(fun(Msg) -> UserPid ! {print_msg, Msg} end, Stored_Messages),
	  
	  {reply, ok, State};
      _->
	  {reply, {ok, error}, State}
  end;

%%------------------------------------------------------------------------------
%%checks if username exist or not then logges him/her out.
%%------------------------------------------------------------------------------
handle_call({log_out,UserName},_From,State)->
    case db_server:logout(UserName) of
	logged_out->
	    ok;
	_ ->
	    error
    end,   
    {reply,ok,State};

%%------------------------------------------------------------------------------
%%as soon as he receives message, starts checking if the user message sent 
%%is online or not. If user is online, then send message instantly 
%%without saving into db to the user. 
%%If he/she is offline, save message in mnesia db. 
%%------------------------------------------------------------------------------
handle_call({send_msg, UserName, Message}, _From, State) ->
    case db_server:check_log_in(UserName) of
	online ->
	    self() ! {print_msg, Message};
	offline->
	    db_server:save_message(UserName, Message),
	    io:format("Message is stored to be gotten later
                       by ~p as soon as ~p is online~n", [UserName, UserName])
    end,
    {reply, ok, State};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
