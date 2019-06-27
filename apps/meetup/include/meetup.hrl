
-record(ws_session, {
    idr,
    id,
    pid,
    type,
    locale,
    time = os:timestamp()
}).