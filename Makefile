GOALS=$(filter-out dep docker, $(MAKECMDGOALS))
.PHONY: $(GOALS) all dep docker

ifeq ($(shell uname -s),Darwin)
NPROC := $(shell sysctl -n hw.ncpu)
else
NPROC := $(shell nproc)
endif


$(GOALS) all:
	omake -w -j $(NPROC) $(GOALS)

dep:
	opam switch set 4.03.0-decomposer --alias 4.03.0
	opam update
	opam install omake
	omake dep

# Aliases to help autocompletion
integration-test:
	omake -w -j $(NPROC) $@

unit-test:
	omake -w -j $(NPROC) $@
