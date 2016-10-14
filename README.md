# Graphite (carbon) metrics reporter Erlang lib.

A dead-simple graphite metrics writer for Erlang, supports only counter and gauge (no histogram).

For richer metrics framework, see [folsom](https://github.com/boundary/folsom) + [folsomite](https://github.com/campanja/folsomite)

## Start a repoter by default

Add to graphiter's app env (in sys.config for example):

```
{writers, [ { writer_1 %% global unique name
            , [ {host, "localhost"} % localhost by default
              , {port, 2003} % 2003 by default
              , {prefix, <<"my-namespace-in-graphite-server">>}
              , {send_timeout, 5000} % 5 seconds by default
              ]
            }
          ]}.
```

## Start a reporter on demand

Add nothing to graphiter app env.

```
graphiter_sup:start_writer(Name, Opts)
```

Where Opts is should of the same pattern as the example given above.

## Bump and counter

```
graphiter:incr_cast(Name, GraphitePath, IncrValue)
```

Where GraphitePath is a (or a list of) dot-separated string binary

## Send counter or gauge value

```
graphiter:cast(Name, GraphitePath, Value)
graphiter:cast(Name, GraphitePath, Value, EpochNow)
```

## Send a batch of values

```
graphiter:cast(Name, Values)
```

Where Values is of spec `[{GraphitePath, Value}]`

