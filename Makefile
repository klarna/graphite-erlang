PROJECT = graphiter
PROJECT_DESCRIPTION = Graphite (carbon) metrics reporter for Erlang
PROJECT_VERSION = 1.0.3

DEPS = supervisor3

dep_supervisor3_commit = 1.1.4

include erlang.mk

MORE_ERLC_OPTS = -DAPPLICATION=graphiter
ERLC_OPTS += $(MORE_ERLC_OPTS)

vsn-check:
	./vsn-check.sh $(PROJECT_VERSION)

hex-publish: distclean
	$(verbose) rebar3 hex publish

