-module(proto).
-export([encode/1, decode/1]).

encode(ping) ->
    <<16#0B:8>>;
encode(count) ->
    <<16#01:8>>;
encode({add, Key, Value}) ->
    <<16#02:8, (byte_size(Key)):32/big, Key/binary, (byte_size(Value)):32/big, Value/binary>>;
encode({update, Key, Value}) ->
    <<16#03:8, (byte_size(Key)):32/big, Key/binary, (byte_size(Value)):32/big, Value/binary>>;
encode({lookup, Key}) ->
    <<16#09:8, (byte_size(Key)):32/big, Key/binary>>;
encode({lend, TimeoutMs, Mode}) ->
    ModeBin = case Mode of block -> 1; poll -> 2 end,
    <<16#04:8, TimeoutMs:64/big, ModeBin:8>>;
encode({repay, LendKey, Key, ChangedValue, Status}) ->
    StatusBin = case Status of penalty -> 1; reward -> 2; front -> 3; drop -> 4 end,
    <<16#05:8, LendKey:64/big, (byte_size(Key)):32/big, Key/binary, (byte_size(ChangedValue)):32/big, ChangedValue/binary, StatusBin:8>>;
encode({heartbeat, LendKey, Key, TimeoutMs}) ->
    <<16#06:8, LendKey:64/big, (byte_size(Key)):32/big, Key/binary, TimeoutMs:64/big>>;
encode(stats) ->
    <<16#07:8>>;
encode(flush) ->
    <<16#0A:8>>;
encode(terminate) ->
    <<16#08:8>>.

decode(<<16#11:8>>) ->
    pong;
decode(<<16#01:8, Count:32/big>>) ->
    {counted, Count};
decode(<<16#02:8>>) ->
    added;
decode(<<16#03:8>>) ->
    kept;
decode(<<16#04:8>>) ->
    updated;
decode(<<16#05:8>>) ->
    not_found;
decode(<<16#0D:8, ValueLen:32/big, Value:ValueLen/binary>>) ->
    {value_found, Value};
decode(<<16#0E:8>>) ->
    value_not_found;
decode(<<16#06:8, LendKey:64/big, KeyLen:32/big, Key:KeyLen/binary, ValueLen:32/big, Value:ValueLen/binary>>) ->
    {lent, LendKey, Key, Value};
decode(<<16#10:8>>) ->
    queue_empty;
decode(<<16#07:8>>) ->
    repaid;
decode(<<16#08:8>>) ->
    heartbeaten;
decode(<<16#0D:8>>) ->
    flushed;
decode(<<16#0A:8, Count:64/big, Add:64/big, Update:64/big, Lookup:64/big, Lend:64/big, Repay:64/big, Heartbeat:64/big, Stats:64/big>>) ->
    {stats_got, [{count, Count},
                 {add, Add},
                 {update, Update},
                 {lookup, Lookup},
                 {lend, Lend},
                 {repay, Repay},
                 {heartbeat, Heartbeat},
                 {stats, Stats}]};
decode(<<16#0C:8>>) ->
    terminated;
decode(Unrecognized) ->
    {decode_error, Unrecognized}.
