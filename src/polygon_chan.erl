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
  case Enabled of
    true ->
      lager:info("Polygon.io client enabled."),
      {ok, ConnPid} = gun:open(Addr, Port, #{transport => ssl, protocols => [http]}),
      {ok, #state{conn_id = ConnPid, tick_fun = TickFun}};
    _ ->
      lager:info("Polygon.io client disabled."),
      {ok, #state{conn_id = undefined, tick_fun = undefined}}
  end.

%%--------------------------------------------------------------------
handle_info({gun_up, _, http}, State) -> do_upgrade_chan(State);
handle_info({gun_ws_upgrade, _, _, _}, State) -> do_auth(State);
handle_info({gun_ws, _, Msg}, State) -> do_message(Msg, State);
handle_info(Other, State) ->
  lager:info("Polygon.in msg: ~p", [Other]),
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
do_auth(State = #state{conn_id = ConnId}) ->
  gun:ws_send(ConnId, {text, auth_msg()}),
  {noreply, State#state{last_subscribe = erlang:localtime(), ticks_since_last_subscribe = 0}}.

%%--------------------------------------------------------------------
%%do_message({text, Data}, State = #state{tick_fun = TF, ticks_since_last_subscribe = TickCnt}) ->
%%  case jiffy:decode(Data, [return_maps]) of
%%    #{
%%      <<"type">> := <<"ticker">>,
%%      <<"product_id">> := Pair,
%%      <<"price">> := Price,
%%      <<"time">> := Time} ->
%%      TF(#{instr => bin2instr(Pair), last_price => bin2float(Price), time => bin2time(Time)});
%%    _ ->
%%      ok
%%  end,
%%  {noreply, State#state{ticks_since_last_subscribe = TickCnt + 1}};
%%%%---
do_message(Other, State) ->
  lager:info("UNEXPECTED Polygon.io MSG: ~p", [Other]),
  {noreply, State}.

%%--------------------------------------------------------------------
%%bin2time(<<Y:4/binary,$-,M:2/binary,$-,D:2/binary,$T,H:2/binary,$:,Mi:2/binary,$:,S:2/binary, _/binary>>) ->
%%  DateTime = {
%%    {binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)},
%%    {binary_to_integer(H), binary_to_integer(Mi), binary_to_integer(S)}
%%  },
%%  calendar:datetime_to_gregorian_seconds(DateTime).

%%--------------------------------------------------------------------
bin2instr(<<F:3/binary, _, S:3/binary>>) -> <<F/binary, S/binary>>.

%%--------------------------------------------------------------------
%%bin2float(Bin) ->
%%  try
%%    binary_to_float(Bin)
%%  catch _:_ ->
%%    float(binary_to_integer(Bin))
%%  end.

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