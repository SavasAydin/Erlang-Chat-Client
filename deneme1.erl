%%% File    : deneme1.erl
%%% Author  : Savas <dartagnan@Dartagnan>
%%% Description : 
%%% Created : 17 Feb 2011 by Savas <dartagnan@Dartagnan>

-module(deneme1).


-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

test_register()->
    ?FORALL({Name,Password},{list(char()),list(int())},
	    chat_client:register_name(Name,Password) == ok).

test_login()->
     ?FORALL({Name,Password},{list(char()),list(int())},
	     begin
		 chat_client:register_name(Name,Password),
		 chat_client:log_in(Name,Password) == ok
	     end).

test_logout()->
     ?FORALL({Name,Password},{list(char()),list(int())},
	     begin
		 chat_client:register_name(Name,Password),
		 chat_client:log_in(Name,Password),
		 chat_client:log_out(Name) == ok
	     end).

test_message()->
    ?FORALL({Name,Message},{list(char()),list(int())},
	    chat_client:send_message(Name,Message) == ok).
    


