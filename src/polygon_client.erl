-module(polygon_client).

%% API exports
-export([test/0]).

%%====================================================================
%% API functions
%%====================================================================


test() ->
  application:ensure_all_started(gun),
  application:set_env(polygon_client, ws_addr, "socket.polygon.io"),
  application:set_env(polygon_client, ws_port, 443),
  application:set_env(polygon_client, enabled, true),
  application:set_env(polygon_client, api_key, "W_W_M4QdPUlb_Art3QvX_f_nLWSTewFVjTBNgi"),
  polygon_chan:start_link(fun(Data) -> io:format("plg GOT: ~p~n", [Data]) end).

%%====================================================================
%% Internal functions
%%====================================================================
