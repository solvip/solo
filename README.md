Solo
====

Solo is an Erlang OTP application that performs duplicate call suppression.

Given key K and a fun F, solo ensures that only one execution of F is in flight at any time.
If a duplicate call for the same key K is performed while F is executing, that caller will
receive a copy of the return value of the original execution of F.

Inspired by the singleflight mechanism in groupcache(https://github.com/golang/groupcache)

## Usage

Simple example usage is as follows:

```erlang
%% Start the application.
1> solo:start().
{ok,[sasl,solo]}

%% Simple calls
2> solo:call({some, key}, fun() -> hello end).
hello

%% Multiple calls from different processes return the same value.
3> spawn(fun() -> io:format("~p got ~p~n", [self(), solo:call(k, fun() -> timer:sleep(3000), erlang:make_ref() end)]) end).
<0.71.0>
4> spawn(fun() -> io:format("~p got ~p~n", [self(), solo:call(k, fun() -> timer:sleep(3000), erlang:make_ref() end)]) end).
<0.74.0>
5> spawn(fun() -> io:format("~p got ~p~n", [self(), solo:call(k, fun() -> timer:sleep(3000), erlang:make_ref() end)]) end).
<0.76.0>
<0.76.0> got #Ref<0.0.0.136>
<0.74.0> got #Ref<0.0.0.136>
<0.71.0> got #Ref<0.0.0.136>
```

In the case where Fun returns exceptionally, solo tries to do the right thing and reraises the error to the caller.
A caveat to be aware of is that part of the stack trace is lost as Fun is actually executed by a freshly spawned process.

### call/2

```erlang
call(Key::term(), Fun::fun()) -> term() | none().
```

Equivalent to `call(Key, Fun, 5000)`.

### call/3

```erlang
call(Key::term(), Fun::fun(), Timeout::non_neg_integer()) -> term() | none().
```

Call `Fun`, a function of arity 0, with duplicate call suppression.
Only a single instance of `Fun` can be in flight at any time for `Key`.
If concurrent callers call call/3 with the same `Key`, they receive a copy of `Fun`'s return value.

If `Fun` doesn't return within `Timeout` milliseconds, it will be killed and call/3 exits with reason `{timeout, _}`.

If `Fun` throws an exception, it will get reraised to all clients.

## Performance

Solo is written to have reasonably low overhead.  You can expect it to be able to throughput well over 100 thousand calls per second on modern hardware.
By default, four processes are pooled, each responsible for roughly 1/4th of the keyspace.
If you think that you need more/less, you can set the application environment variable `pool_size` to the desired value.

## Contributing

1. Fork it!
2. Create your feature branch: `git checkout -b my-new-feature`
3. Commit your changes: `git commit -am 'Add some feature'`
4. Push to the branch: `git push origin my-new-feature`
5. Submit a pull request :D

## License

Copyright (c) 2015 Sölvi Páll Ásgeirsson <solvip@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

