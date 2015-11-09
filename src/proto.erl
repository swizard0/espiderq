-module(proto).
-export([encode/1, decode/1]).

encode(count) -> 
    <<16#01:8, 16#01:8>>;
encode({add, Data}) ->
    <<16#01:8, 16#02:8, Data/binary>>;
encode({lend, TimeoutMs}) ->
    <<16#01:8, 16#03:8, TimeoutMs:64/big>>;
encode({repay, Id, penalty}) ->
    <<16#01:8, 16#04:8, Id:32/big, 16#01:8>>;
encode({repay, Id, reward}) ->
    <<16#01:8, 16#04:8, Id:32/big, 16#02:8>>;
encode({repay, Id, front}) ->
    <<16#01:8, 16#04:8, Id:32/big, 16#03:8>>;
encode(stats) ->
    <<16#01:8, 16#05:8>>.

decode(<<16#01:8, 16#01:8, Count:32/big>>) ->
    {counted, Count};
decode(<<16#01:8, 16#02:8, Id:32/big>>) ->
    {added, Id};
decode(<<16#01:8, 16#03:8, Id:32/big, Data/binary>>) ->
    {lent, Id, Data};
decode(<<16#01:8, 16#04:8>>) ->
    repaid;
decode(<<16#01:8, 16#05:8, Count:64/big, Add:64/big, Lend:64/big, Repay:64/big, Stats:64/big>>) ->
    {stats, [{count, Count}, {add, Add}, {lend, Lend}, {repay, Repay}, {stats, Stats}]};
decode(Unrecognized) ->
    {decode_error, Unrecognized}.
