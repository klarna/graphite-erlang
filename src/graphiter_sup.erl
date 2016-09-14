-module(graphiter_sup).
-behaviour(supervisor3).

-export([ init/1
        , post_init/1
        , start_link/0
        , start_writer/2
        ]).

-define(SUP, ?MODULE).
-define(DEFAULT_DELAY_SEC, 10).

start_link() ->
  supervisor3:start_link({local, ?SUP}, ?MODULE, []).

start_writer(Name, Opts) ->
   supervisor3:start_child(?SUP, writer({Name, Opts})).

init([]) ->
  {ok, {{one_for_one, 0, 1}, writers()}}.

writers() ->
  %% start writers pre-defined in app env
  Writers = application:get_env(?APPLICATION, writers, []),
  lists:map(fun writer/1, Writers).

writer({Name, Opts}) ->
  Delay = proplists:get_value(restart_delay, Opts, ?DEFAULT_DELAY_SEC),
  { Name
  , {graphiter_writer, start_link, [Name, Opts]}
  , {transient, Delay}
  , 5000
  , worker
  , [graphiter_writer]
  }.

post_init(_) -> erlang:error(unexpected).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
