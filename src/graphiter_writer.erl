-module(graphiter_writer).

-behaviour(gen_server).

%% API
-export([start_link/2, cast/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(DEFAULT_HOST, "localhost").
-define(DEFAULT_PORT, 2003).
-define(SOCKET_INIT_TIMEOUT, 5000).
-define(SOCKET_SEND_TIMEOUT, 5000).
-define(undef, undefined).

-type name() :: graphiter:name().
-type path() :: binary(). %% unified format
-type value() :: graphiter:value().
-type epoch() :: graphiter:epoch().

-record(state,
        { name   :: name()
        , prefix :: ?undef | path()
        , socket :: pid()
        , opts   :: [{atom(), term()}]
        }).

%%%_* APIs =====================================================================

-spec start_link(atom(), list()) -> {ok, pid()}.
start_link(Name, Opts) ->
  gen_server:start_link({local, Name}, ?MODULE, [Name, Opts], []).

-spec cast(atom(), [{path(), value()}], epoch()) -> ok.
cast(Name, PathValues, Epoch) ->
  gen_server:cast(Name, {send, PathValues, Epoch}).

%%%_* gen_server callbacks =====================================================

init([Name, Opts]) ->
  ets:new(Name, [public, named_table, {write_concurrency, true}]),
  ok = gen_server:cast(self(), post_init),
  %% if prefix is not given, use the registration name by default
  Prefix = proplists:get_value(prefix, Opts, Name),
  State = #state
          { name   = Name
          , prefix = graphiter:path(Prefix)
          , socket = ?undef %% initialized in post_init
          , opts   = Opts
          },
  {ok, State}.

post_init(#state{opts = Opts} = State) ->
  Host = proplists:get_value(host, Opts, ?DEFAULT_HOST),
  Port = proplists:get_value(port, Opts, ?DEFAULT_PORT),
  %% use delay_send to send larger tcp packets
  SocketOpts = [ {delay_send, true}
               , {send_timeout, ?SOCKET_SEND_TIMEOUT}
               ],
  case gen_tcp:connect(Host, Port, SocketOpts, ?SOCKET_INIT_TIMEOUT) of
    {ok, Socket} ->
      State#state{socket = Socket};
    {error, Reason} ->
      exit({failed_to_connect, Host, Port, Reason})
  end.

handle_call(Call, _From, State) ->
  {reply, {error, {unexpected_call, Call}}, State}.

handle_cast({send, PathValues, Epoch}, State) ->
  ok = handle_send(State, PathValues, Epoch),
  {noreply, State};
handle_cast(post_init, State) ->
  {noreply, post_init(State)};
handle_cast(_Cast, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(Reason, State) ->
  ok = log_error(State#state.name, Reason),
  ok = close_socket(State#state.socket),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%_* Internal functions =======================================================

close_socket(?undef) ->
  ok;
close_socket(Socket) ->
  _ = gen_tcp:close(Socket),
  ok.

handle_send(#state{socket = Socket, prefix = Prefix}, PathValues, Epoch) ->
  case send(Socket, fmt_lines(Prefix, PathValues, Epoch)) of
    ok              -> ok;
    {error, Reason} -> exit({socket_error, Reason})
  end.

send(_Socket, <<>>) -> ok;
send(Socket, Line)  -> gen_tcp:send(Socket, Line).


-spec fmt_lines(?undef | path(), [{path(), value()}], epoch()) -> iodata().
fmt_lines(Prefix, PathValues, Epoch) ->
  iolist_to_binary(
    lists:map(fun({Path, Value}) ->
                fmt_line(Prefix, Path, Value, Epoch)
              end, PathValues)).

%% @private Format <metric path> <metric value> <metric timestamp>
%% See http://graphite.readthedocs.io/en/latest/feeding-carbon.html
%% @end
-spec fmt_line(?undef | path(), path(), value(), epoch()) -> iodata().
fmt_line(Prefix, Path, Value, Epoch) ->
  [fmt_path(Prefix, Path), " ",
   fmt_value(Value), " ",
   integer_to_list(Epoch), "\r\n"].

-spec fmt_value(number()) -> iodata().
fmt_value(I) when is_integer(I) -> integer_to_list(I);
fmt_value(I) when is_float(I)   -> float_to_list(I).

-spec fmt_path(?undef | binary(), binary()) -> binary().
fmt_path(?undef, Path) -> Path;
fmt_path(Prefix, Path) -> iolist_to_binary([Prefix, ".", Path]).

is_error(normal)        -> false;
is_error(shutdown)      -> false;
is_error({shutdown, _}) -> false;
is_error(_)             -> true.

log_error(Name, Reason) ->
  case is_error(Reason) of
    true ->
      error_logger:error_msg("graphiter_writer ~p terminated.\n"
                             "reason: ~p", [Name, Reason]);
    false ->
      ok
  end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
