-module(graphiter).

-export([ start/2
        , cast/2
        , cast/3
        , cast/4
        ]).

-export_type([ name/0
             , path/0
             , value/0
             , epoch/0
             ]).

-type name() :: atom().
-type path() :: atom() | binary() | [atom()] | [binary()].
-type value() :: number().
-type epoch() :: pos_integer().

%%%_* APIs =====================================================================

%% @doc Start a new metrics writer under graphite_erlang's supervisor.
%% Supported options:
%%   host (default = "localhost")
%%      graphite (carbon) server
%%   port (default = 2003)
%%      graphite (carbon) plaintext protocol listener
%%   prefix (default = Name)
%%      The metrics namespace prefix, in case of 'undefined' given in Opts,
%%      no prefix is prepended to the metrics names
%%   restart_delay (default = 10)
%%      Seconds to delay after writer process crash (e.g. due to socket error)
%% @end
-spec start(name(), [{atom(), term()}]) -> {ok, pid()} | {error, any()}.
start(Name, Opts) ->
  _ = application:ensure_all_started(?APPLICATION),
  graphiter_sup:start_writer(Name, Opts).

%% @doc Send a batch of metrics data.
cast(Name, PathValues) ->
  graphiter_writer:cast(Name, PathValues, epoch()).

%% @doc Send metric data with current timestamp @see cast/4
-spec cast(name(), path(), value()) -> ok.
cast(Name, Path, Value) ->
  cast(Name, Path, Value, epoch()).

%% @doc Send <prefix>.<path> <metric value> <metric timestamp> to graphite.
%% where prefix is taken from start options (@see start/1.)
%% @end
-spec cast(name(), path(), value(), epoch()) -> ok.
cast(Name, Path, Value, Epoch) ->
  graphiter_writer:cast(Name, [{Path, Value}], Epoch).

%%%_* Internal functions =======================================================

epoch() ->
  {Ms, S, _} = os:timestamp(),
  Ms * 1000000 + S.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
