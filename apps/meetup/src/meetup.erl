%% Feel free to use, reuse and abuse the code in this file.

-module(meetup).

%% API.
-export([init/0,
         generate_test/1,
         start/0]).

% API.
-include_lib("meetup/include/meetup.hrl").

-include_lib("eunit/include/eunit.hrl").


start() ->
    application:start(mnesia),
    application:start(meetup_app).

init() ->
    mnesia:create_schema([node()]),
    mnesia:create_table(ws_session,
        [{ram_copies, [node() | nodes()]},
            {attributes, record_info(fields, ws_session)}]),
    mnesia:add_table_index(ws_session, id),
    mnesia:add_table_index(ws_session, type),
    mnesia:add_table_index(ws_session, pid).


write_test() ->
    Id = <<"id">>,
    Mac = <<"Mac">>,
    Session = #ws_session{idr = {Id, Mac}, id = Id, type = <<"hub">>, pid = pid},
    mnesia:dirty_write(Session).

read_test() ->
    Pid = pid,
    mnesia:dirty_index_read(ws_session, Pid, pid).


delete_object_type_test() ->
    Pid = pid,
    List = mnesia:dirty_index_read(ws_session, Pid, pid),
    [mnesia:dirty_delete_object(E#ws_session{type= <<"hub1">>}) || E <- List].


delete_object_test() ->
    Pid = pid,
    List = mnesia:dirty_index_read(ws_session, Pid, pid),
    [mnesia:dirty_delete_object(E) || E <- List].



generate_test(0) ->
    ok;
generate_test(N) ->
    Id = <<"id">>,
    Mac = <<"Mac">>,
    Session = #ws_session{idr = {rand:uniform(), Mac}, id = Id, type = <<"hub">>, pid = pid1},
    mnesia:dirty_write(Session),
    Session1 = #ws_session{idr = {rand:uniform(), Mac}, id = Id, type = <<"user">>, pid = pid1},
    mnesia:dirty_write(Session1),
    generate_test(N-1).


