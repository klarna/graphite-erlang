PROJECT = graphiter
PROJECT_DESCRIPTION = Graphite (carbon) metrics reporter for Erlang
PROJECT_VERSION = 1.0.5

DEPS = supervisor3

dep_supervisor3 = hex 1.1.5

include erlang.mk

MORE_ERLC_OPTS = -DAPPLICATION=graphiter
ERLC_OPTS += $(MORE_ERLC_OPTS)

vsn-check:
	./vsn-check.sh $(PROJECT_VERSION)

hex-publish: distclean
	$(verbose) rebar3 hex publish

