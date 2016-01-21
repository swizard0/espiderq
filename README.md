# spiderq protocol for Erlang

## Summary

Protocol implementation for [spiderq](https://github.com/swizard0/spiderq) server as Erlang module.

## Installation

Just include this repository in your project's `rebar.config` file and run `rebar get-deps`. See [rebar](https://github.com/rebar/rebar) for more details on how to use rebar for Erlang project management.

## Usage

```
1> {ok, _} = application:ensure_all_started(espiderq).
{ok,[sasl,syntax_tools,compiler,goldrush,lager,
     gen_listener_tcp,ezmq,espiderq]}

2> {ok, _} = espiderq:start_link(espiderq, [ tcp, "127.0.0.1", 43777, [] ]).
{ok,<0.93.0>}

3> espiderq:req(espiderq, ping).
pong

4> espiderq:req(espiderq, count).
{counted,0}

5> espiderq:req(espiderq, {add, <<"my_key">>, <<"my_entry">>}).
added

6> espiderq:req(espiderq, {lookup, <<"my_key">>}).
{value_found,<<"my_entry">>}

7> espiderq:req(espiderq, {lend, 5000, block}).
{lent,2,<<"my_key">>,<<"my_entry">>}

8> espiderq:req(espiderq, {repay, 2, <<"my_key">>, <<"new_value">>, penalty}).
repaid

9> espiderq:req(espiderq, {lookup, <<"my_key">>}).
{value_found,<<"new_value">>}
```

## License

The MIT License (MIT)

Copyright (c) 2016 Alexey Voznyuk

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.