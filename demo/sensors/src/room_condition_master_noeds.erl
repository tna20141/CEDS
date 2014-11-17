-module(room_condition_master_noeds).
-include("general.hrl").
-behavior(gen_fsm).

-export([start_link/1]).

start_link(Data) -> ok.