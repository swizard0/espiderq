# spiderq protocol for Erlang

## Summary

Protocol implementation for [spiderq](https://github.com/swizard0/spiderq) server as Erlang module.

## Usage

```erlang
1> {ok, _} = application:ensure_all_started(espiderq).

2> {ok, _} = espiderq:start_link(spiderq, [tcp, "localhost", 53713]).
starting espiderq under the name of espiderq
espiderq started, pid: <0.474.0>, socket: <0.475.0>, client: <0.476.0>
{ok,<0.474.0>}

3> espiderq:req(spiderq, ping).
pong

4> espiderq:req(spiderq, count).
{counted,0}

5> espiderq:req(spiderq, {add, <<"my_key">>, <<"my_entry">>, head}).
added

6> espiderq:req(spiderq, {lookup, <<"my_key">>}).
{value_found,<<"my_entry">>}

7> espiderq:req(spiderq, {lend, 5000, block}).
{lent,2,<<"my_key">>,<<"my_entry">>}

8> espiderq:req(spiderq, {repay, 2, <<"my_key">>, <<"new_value">>, penalty}).
repaid

9> espiderq:req(spiderq, {lookup, <<"my_key">>}).
{value_found,<<"new_value">>}

10> espiderq:req(spiderq, {remove, <<"my_key">>}).
removed

11> espiderq:req(spiderq, {lookup, <<"my_key">>}).
value_not_found
```

## License

The MIT License (MIT)

Copyright (c) 2016 Alexey Voznyuk, 2021 Alexander Kyusev

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
