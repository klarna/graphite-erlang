-module(graphiter).

-export([ start/2
        , cast/2
        , cast/3
        , cast/4
        , incr_cast/3
        , path/1
        , get_writers/0
        ]).

-export_type([ name/0
             , path/0
             , value/0
             , epoch/0
             ]).

-type name() :: atom().
-type path_seg() :: atom() | binary().
-type path() :: path_seg() | [path_seg()].
-type value() :: number().
-type epoch() :: pos_integer().

-ifndef(APPLICATION).
-define(APPLICATION, ?MODULE).
-endif.

%%%_* APIs =====================================================================

%% @doc Get configured writers.
-spec get_writers() -> [{name(), proplists:proplist()}].
get_writers() ->
  application:get_env(?APPLICATION, writers, []).

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
cast(Name, PathValues0) ->
  PathValues =
    lists:map(fun({Path, Value}) ->
                {path(Path), Value}
              end, PathValues0),
  graphiter_writer:cast(Name, PathValues, epoch()).

%% @doc Send metric data with current timestamp @see cast/4
-spec cast(name(), path(), value()) -> ok.
cast(Name, Path, Value) ->
  cast(Name, Path, Value, epoch()).

%% @doc Send `<prefix>.<path> <metric value> <metric timestamp>' to graphite.
%% where prefix is taken from start options (@see start/1.)
%% @end
-spec cast(name(), path(), value(), epoch()) -> ok.
cast(Name, Path, Value, Epoch) ->
  graphiter_writer:cast(Name, [{path(Path), Value}], Epoch).

%% @doc Increment a counter and send to graphite.
-spec incr_cast(name(), path(), value()) -> ok.
incr_cast(Name, Path0, IncrValue) when is_integer(IncrValue) ->
  Path = path(Path0),
  Default = {Path, 0},
  try
    Value = ets:update_counter(Name, Path, IncrValue, Default),
    graphiter_writer:cast(Name, [{Path, Value}], epoch())
  catch error : badarg ->
    ok
  end.

%% @doc Make a dot infixed graphite metric value path.
-spec path(path()) -> iodata() | no_return().
path(A) when is_atom(A) ->
  path(atom_to_binary(A, utf8));
path(<<>>) ->
  erlang:error(bad_path);
path(B) when is_binary(B) ->
  case binary:match(B, <<" ">>) of
    nomatch -> B;
    _       -> erlang:error(bad_path)
  end;
path([_|_] = L) ->
  infix([path(I) || I <- L]).

%%%_* Internal functions =======================================================

-spec infix([atom() | binary()]) -> iolist().
infix([_] = L) -> L;
infix([H|T])   -> [H, $. | infix(T)].

epoch() ->
  {Ms, S, _} = os:timestamp(),
  Ms * 1000000 + S.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
