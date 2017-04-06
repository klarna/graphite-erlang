# Graphite (carbon) Metrics Reporter For Erlang.

A dead-simple graphite metrics writer for Erlang, supports only counter and gauge (no histogram).

For richer metrics framework, see [folsom](https://github.com/boundary/folsom) + [folsomite](https://github.com/campanja/folsomite) or [exometer](https://github.com/Feuerlabs/exometer)

## Start a Repoter by Default

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

## Start a Reporter on Demand

No need for config in app env or sys.config.

```
graphiter_sup:start_writer(Name, Opts)
```

Where Opts is of the same pattern as the example above.

## Bump Counters

```
graphiter:incr_cast(Name, GraphitePath, IncrValue)
```

Where GraphitePath is a (or a list of) dot-separated string binary

## Send Counter or Gauge Values

```
graphiter:cast(Name, GraphitePath, Value)
graphiter:cast(Name, GraphitePath, Value, EpochNow)
```

## Send a Batch of Values

```
graphiter:cast(Name, Values)
```

Where `Values` is of spec `[{GraphitePath, Value}]`

