VERSION := 0.1

export ERLANG_ROOT := $(shell echo 'io:format("~s~n", [code:root_dir()]).' | erl | awk '/1>/ {print $$NF;}')
export ERLANG_LIB := $(shell echo 'io:format("~s~n", [code:lib_dir()]).' | erl | awk '/1>/ {print $$NF;}')
export PROJECT_ROOT := $(shell pwd)
export PROJECT_NAME := $(shell basename $(PROJECT_ROOT))
export INSTALL_TARGET := $(ERLANG_LIB)/$(PROJECT_NAME)-$(VERSION)

.PHONY: doc test

all: compile

install: all
	-@echo Installing to $(INSTALL_TARGET)
	-@rm -rf "$(INSTALL_TARGET)" >/dev/null 2>&1
	@find ebin include src -print | cpio -pdum --quiet "$(INSTALL_TARGET)"

compile: deps
	rebar compile

deps:
	rebar get-deps

doc:
	rebar doc

clean:
	rebar clean

dialyzer:
	rebar analyze

test: all
	rebar eunit

demo: all
	demo/run_demo
