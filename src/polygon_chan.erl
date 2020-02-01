%%%-------------------------------------------------------------------
%%% @author an
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Nov 2017 14:39
%%%-------------------------------------------------------------------
-module(polygon_chan).
-author("an").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-type polygon_tick() :: #{
instr => binary(),
time => pos_integer(),
last_price => float()
}.

-type tick_fun() :: fun((Tick :: polygon_tick()) -> ok).

-export_type([polygon_tick/0, tick_fun/0]).

-record(state, {
  conn_id :: pid(),
  tick_fun :: tick_fun(),
  timestamp_offset :: non_neg_integer(),
  last_subscribe :: calendar:datetime(),
  ticks_since_last_subscribe = 0 :: non_neg_integer()
}).

-compile([{parse_transform, lager_transform}]).
%%%===================================================================
%%% API
%%%===================================================================
-spec(start_link(TickFun :: tick_fun()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(TickFun) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [TickFun], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([TickFun]) ->
  Addr = rz_util:get_env(polygon_client, ws_addr),
  Port = rz_util:get_env(polygon_client, ws_port),
  Enabled = rz_util:get_env(polygon_client, enabled),
  TimestampOffset = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
  case Enabled of
    true ->
      lager:info("Polygon.io client enabled."),
      {ok, ConnPid} = gun:open(Addr, Port, #{transport => ssl, protocols => [http]}),
      {ok, #state{conn_id = ConnPid, tick_fun = TickFun, timestamp_offset = TimestampOffset}};
    _ ->
      lager:info("Polygon.io client disabled."),
      {ok, #state{conn_id = undefined, tick_fun = undefined, timestamp_offset = TimestampOffset}}
  end.

%%--------------------------------------------------------------------
handle_info({gun_up, _, http}, State) -> do_upgrade_chan(State);
handle_info({gun_ws_upgrade, _, _, _}, State) -> {noreply, State};
handle_info({gun_ws, _, {text, Data}}, State) ->
  case catch jiffy:decode(Data, [return_maps]) of
    MapMsg when is_map(MapMsg) ->
      do_message(MapMsg, State);
    MsgList when is_list(MsgList) ->
      lists:foldr(fun(M, {noreply, St}) -> do_message(M, St) end, {noreply, State}, MsgList);
    Else ->
      lager:warning("cannot decode Polygon message: ~p, ~p", [Data, Else]),
      {noreply, State}
  end;
handle_info(Other, State) ->
  lager:warning("unexpected websocket  msg: ~p", [Other]),
  {noreply, State}.

%%--------------------------------------------------------------------
handle_call(_Request, _From, _State) -> exit(handle_call_unsupported).
handle_cast(_Request, _State) -> exit(handle_cast_unsupported).
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_upgrade_chan(State = #state{conn_id = ConnId}) ->
  gun:ws_upgrade(ConnId, "/forex"),
  {noreply, State}.

%%--------------------------------------------------------------------
do_message(#{<<"ev">> := <<"status">>, <<"status">> := <<"connected">>}, State) ->
  lager:info("Connected to Polygon.io"),
  gun:ws_send(State#state.conn_id, {text, auth_msg()}),
  {noreply, State#state{last_subscribe = erlang:localtime(), ticks_since_last_subscribe = 0}};
do_message(#{<<"ev">> := <<"status">>, <<"status">> := <<"auth_success">>}, State) ->
  lager:info("Polygon connection authenticated"),
  gun:ws_send(State#state.conn_id, {text, subscribe_msg()}),
  {noreply, State};
do_message(#{<<"ev">> := <<"C">>, <<"p">> := Pair, <<"b">> := Bid, <<"a">> := _Ask, <<"t">> := Timestamp}, State = #state{ticks_since_last_subscribe = TickCnt, tick_fun = TF}) ->
  TF(#{instr => bin2instr(Pair), last_price => bin2float(Bid), time => bin2time(Timestamp, State)}),
  {noreply, State#state{ticks_since_last_subscribe = TickCnt + 1}};
do_message(Other, State) ->
  lager:debug("Got unprocessed Polygon msg: ~p", [Other]),
  {noreply, State}.

%%--------------------------------------------------------------------


%%--------------------------------------------------------------------
bin2time(TS, #state{timestamp_offset = Offset}) when is_integer(TS) -> Offset + (TS div 1000);
bin2time(TSBin, State) when is_binary(TSBin)-> bin2time(binary_to_integer(TSBin), State).

%%--------------------------------------------------------------------
bin2instr(<<F:3/binary, _, S:3/binary>>) -> <<F/binary, S/binary, ".FXCM">>.

%%--------------------------------------------------------------------
bin2float(F) when is_float(F)-> F;
bin2float(Bin) ->
  try
    binary_to_float(Bin)
  catch _:_ ->
    float(binary_to_integer(Bin))
  end.

%%--------------------------------------------------------------------
auth_msg() ->
  APIKey = rz_util:get_env(polygon_client, api_key),
  iolist_to_binary(
    [

      "{
      \"action\": \"auth\",
      \"params\": \"", APIKey,
      "\"
    }"
    ]
  ).

%%--------------------------------------------------------------------
subscribe_msg() ->
  Currencies = rz_util:get_env(polygon_client, currencies),
  TrFun = fun(<<F:3/binary, S:3/binary, ".FXCM">>) -> <<"C.", F/binary, "/", S/binary>> end,
  <<_,Text/binary>> = iolist_to_binary([[",", TrFun(C)] || C <- Currencies]),
  iolist_to_binary(
    [
      "{
      \"action\": \"subscribe\",
      \"params\": \"", Text,
      "\"
      }"
    ]
  ).
