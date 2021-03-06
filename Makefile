# -----------------------------------------------------------------------------
#
# Phonostroma
#
# Copyright (c) 2010 Tim Watson (watson.timothy@gmail.com)
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
# -----------------------------------------------------------------------------

ERL ?= `which erl`
ERL_LIBS := $(shell echo "./deps:`echo $$ERL_LIBS`")
VERBOSE ?= ""
HERE := $(shell pwd)
BUILD := $(HERE)/build
ERTS_VSN := $(shell escript scripts/checkvsn "5.7.5")
ifeq ($(ERTS_VSN), 0)
    INCL_TYPES = "-b"
endif
INCL_TYPES ?= "NOTYPES=1"

all: check info
    $(info ready to run the test, package and/or install tasks)

info:
	$(info erl program located at $(ERL))
	$(info ERL_LIBS set to $(ERL_LIBS))

deps: build
	mkdir -p $@

build:
	mkdir -p $@

deps/exmpp: deps
	@(echo `env ERL_LIBS=$(ERL_LIBS) escript scripts/checkdeps "exmpp"` | \
        env HOME=$(BUILD) ./epm install processone/exmpp \
            --tag v0.9.3 \
            --prebuild-command "autoreconf -vif" \
            --build-command "configure && make" \
            --config-set build_dir $(HERE)/build \
            --config-set install_dir $(HERE)/deps $$VERBOSE)

deps/ejabberd: deps
	@(echo `env ERL_LIBS=$(ERL_LIBS) escript scripts/checkdeps "ejabberd"` | \
        env HOME=$(BUILD) ./epm install processone/ejabberd \
            --tag v2.1.4 \
            --prebuild-command "cd src && ./configure" \
            --build-command "cd src && make && mkdir ../ebin && cp ejabberd.app ../ebin/" \
            --config-set build_dir $(BUILD) \
            --config-set install_dir $(HERE)/deps $$VERBOSE)

deps/proper: deps
	@(echo `env ERL_LIBS=$(ERL_LIBS) escript scripts/checkdeps "proper"` | \
        env HOME=$(BUILD) ./epm install manopapad/proper \
            --build-command "make $(INCL_TYPES) && rm src/proper.app.src" \
            --config-set build_dir $(BUILD) \
            --config-set install_dir $(HERE)/deps $$VERBOSE)

deps/hamcrest/include/hamcrest.hrl: 
	cd deps/hamcrest; env ERL_LIBS=$$ERL_LIBS make compile VERBOSE=$(VERBOSE)

check: deps/proper deps/exmpp deps/ejabberd
	@(env ERL_LIBS=$$ERL_LIBS ./rebar $$VERBOSE get-deps check-deps)

compile: check
	@(env ERL_LIBS=$$ERL_LIBS ./rebar $$VERBOSE compile skip_deps=true)

clean:
	@(echo `env ERL_LIBS=$(ERL_LIBS) escript scripts/checkdeps "exmpp"` | \
        env HOME=$(BUILD) ./epm remove exmpp \
            --config-set build_dir $(BUILD) \
            --config-set install_dir $(HERE)/deps $$VERBOSE)
	@(echo `env ERL_LIBS=$(ERL_LIBS) escript scripts/checkdeps "proper"` | \
        env HOME=$(BUILD) ./epm remove proper \
            --config-set build_dir $(BUILD) \
            --config-set install_dir $(HERE)/deps $$VERBOSE)
	@(echo `env ERL_LIBS=$(ERL_LIBS) escript scripts/checkdeps "ejabberd"` | \
        env HOME=$(BUILD) ./epm remove ejabberd \
            --config-set build_dir $(BUILD) \
            --config-set install_dir $(HERE)/deps $$VERBOSE)
	@(./rebar $$VERBOSE clean skip_deps=true)

edoc:
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[{preprocess, true},{includes, ["."]}]'

test: compile deps/hamcrest/include/hamcrest.hrl
	@(env ERL_LIBS=$$ERL_LIBS ./rebar $$VERBOSE ct skip_deps=true)
