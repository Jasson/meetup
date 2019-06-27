%% Feel free to use, reuse and abuse the code in this file.

-module(octopus).

%% API.
-export([start_deps/0,
         start/0]).

% API.
-export([timestamp/0]).
-include_lib("vtx_common/include/vtx.hrl").

ensure_started(App) ->
    error_logger:info_msg("~p:ensure_started ~p App= ~p ~n", [?MODULE, ?LINE, App]),
    io:format("~p:ensure_started ~p App= ~p ~n", [?MODULE, ?LINE, App]),
    case application:ensure_all_started(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok;
	Error  ->
	    io:format("~p:ensure_started ~p App= ~p, Error=~p ~n", [?MODULE, ?LINE, App, Error]),
	    error_logger:info_msg("~p:ensure_started ~p App= ~p, Error=~p ~n", [?MODULE, ?LINE, App, Error])
    end.

start_deps() ->
  ensure_started(inets),
  ensure_started(asn1),
  ensure_started(crypto),
  ensure_started(public_key),
  ensure_started(ssl),
  ensure_started(sasl),
  ensure_started(fluxer),               % influxdb 驱动
  ensure_started(os_mon),
  ensure_started(syntax_tools),
  ensure_started(compiler),
  ensure_started(goldrush),
  ensure_started(lager),
  ensure_started(apns),
  ensure_started(eapns),
  %% redis
  %ensure_started(vtx_reddy),
  %% mongo
  ensure_started(mongodb),
  ensure_started(mongrel),
  %% voltdb
  %ensure_started(erlvolt),
  %ensure_started(vtx_voltrel),
  %% rafter
  %ensure_started(rafter),
  %% riak_pipe
  %ensure_started(mochiweb),
  %ensure_started(webmachine),
  %ensure_started(riak_sysmon),
  %ensure_started(riak_core),
  %ensure_started(riak_pipe),
  %ensure_started(mnesia),
  init_mnesia(),
  %% cowboy
  ensure_started(ranch),
  ensure_started(cowlib),
  ensure_started(cowboy),
  %%% gproc
  %ensure_started(gproc),
  %% octopus
  ok.
  %ensure_started(octopus).
start() ->
  start_deps(),
  ensure_started(octopus).


%% @doc os:timestamp in milliseconds
-spec timestamp() -> pos_integer().
timestamp() ->
  {_, _, MicroSecs} = os:timestamp(),
  Millis = erlang:trunc(MicroSecs/1000),
  calendar:datetime_to_gregorian_seconds(
    calendar:universal_time()) * 1000 + Millis.


init_mnesia() ->
    Schema = mnesia:create_schema([node()]),
    ensure_started(mnesia),
    io:format("ResultSchema ===================================== ~p", [Schema]),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    mnesia:create_table(agent_session,                        
                        [{ram_copies, [node()|nodes()]},        
                         {attributes, record_info(fields, agent_session)}]),

    mnesia:create_table(weather_cache,                        
                        [{ram_copies, [node()|nodes()]},        
                         {attributes, record_info(fields, weather_cache)}]),
    
    mnesia:create_table(weather_aqi,                        
                        [{ram_copies, [node()|nodes()]},        
                         {attributes, record_info(fields, weather_aqi)}]),
    
    mnesia:create_table(ws_session,                        
                        [{ram_copies, [node()|nodes()]},        
                         {attributes, record_info(fields, ws_session)}]),
    mnesia:add_table_index(ws_session, id),
    mnesia:add_table_index(ws_session, pid),
    mnesia:create_table(octopus_gadget_route,                        
                        [{ram_copies, [node()|nodes()]},        
                         {attributes, record_info(fields, octopus_gadget_route)}]),
    mnesia:add_table_index(octopus_gadget_route, hub_id),
    mnesia:add_table_index(octopus_gadget_route, user_id),
    mnesia:create_table(octopus_third_gadget_status,                        
                        [{ram_copies, [node()|nodes()]},        
                         {attributes, record_info(fields, octopus_third_gadget_status)}]),
    mnesia:create_table(octopus_log_config,                        
                        [{ram_copies, [node()|nodes()]},        
                         {attributes, record_info(fields, octopus_log_config)}]),

    mnesia:create_table(debug_info,
                         [{ram_copies, [node()|nodes()]},
                          {attributes, record_info(fields, debug_info)}]),

    mnesia:create_table(ts_ver_cache,
                         [{ram_copies, [node()|nodes()]},
                          {attributes, record_info(fields, ts_ver_cache)}]),
    mnesia:add_table_index(ts_ver_cache, user_id),
    mnesia:add_table_index(ts_ver_cache, ts_id),

    mnesia:create_table(queue_message,
                         [{ram_copies, [node()|nodes()]},
                          {attributes, record_info(fields, queue_message)}]),
    mnesia:add_table_index(queue_message, client_id),
 
    mnesia:create_table(octopus_session,
                         [{ram_copies, [node()|nodes()]},
                          {type,set},   % default is set
                          {attributes, record_info(fields, octopus_session)}]),   
    mnesia:add_table_index(octopus_session, user_mac),

    mnesia:create_table(gadget_attr,
                         [{ram_copies, [node()|nodes()]},
                          {attributes, record_info(fields, gadget_attr)}]),

    mnesia:create_table(octopus_share_code_info,
                        [{ram_copies, [node()|nodes()]},
                         {attributes, record_info(fields, octopus_share_code_info)}]),

    mnesia:create_table(octopus_share_info_cache,
                        [{ram_copies, [node()|nodes()]},
                         {attributes, record_info(fields, octopus_share_info_cache)}]),
    mnesia:add_table_index(octopus_share_info_cache, user_id),
    mnesia:add_table_index(octopus_share_info_cache, user_member),
    mnesia:add_table_index(octopus_share_info_cache, gadget_member),
    mnesia:add_table_index(octopus_share_info_cache, gadget_id),
    mnesia:add_table_index(octopus_share_info_cache, '_id'),
    
    mnesia:create_table(octopus_gadget_type_zip,
                        [{ram_copies, [node()|nodes()]},
                         {attributes, record_info(fields, octopus_gadget_type_zip)}]),
    
    mnesia:create_table(hub_session,
                        [{ram_copies, [node()|nodes()]},
                         {attributes, record_info(fields, hub_session)}]),
    mnesia:create_table(octopus_kv,
                        [{ram_copies, [node()|nodes()]},
                         {attributes, record_info(fields, octopus_kv)}]),
    
    mnesia:create_table(user_session,
                        [{ram_copies, [node()|nodes()]},
                         {attributes, record_info(fields, user_session)}]),
    
    mnesia:create_table(push_config_cache,                        
                        [{ram_copies, [node()|nodes()]},        
                         {attributes, record_info(fields, push_config_cache)}]).
 

