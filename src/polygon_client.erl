-module(polygon_client).

%% API exports
-export([test/0]).

%%====================================================================
%% API functions
%%====================================================================


test() ->
  application:ensure_all_started(gun),
  application:ensure_all_started(lager),
  application:set_env(polygon_client, ws_addr, "socket.polygon.io"),
  application:set_env(polygon_client, ws_port, 443),
  application:set_env(polygon_client, enabled, true),
  application:set_env(polygon_client, api_key, "W_W_M4QdPUlb_Art3QvX_f_nLWSTewFVjTBNgi"),
  application:set_env(polygon_client, currencies, [
    <<"AUDJPY.FXCM">>,
    <<"AUDUSD.FXCM">>,
    <<"CADJPY.FXCM">>,
    <<"CHFJPY.FXCM">>,
    <<"EURGBP.FXCM">>,
    <<"EURJPY.FXCM">>,
    <<"EURUSD.FXCM">>,
    <<"GBPJPY.FXCM">>,
    <<"GBPUSD.FXCM">>,
    <<"NZDJPY.FXCM">>,
    <<"NZDUSD.FXCM">>,
    <<"USDJPY.FXCM">>
  ]),
  polygon_chan:start_link(fun(Data) -> io:format("plg GOT: ~p~n", [Data]) end).

%%====================================================================
%% Internal functions
%%====================================================================
