ifeq ($(shell uname -s),Darwin)
  NPROC := $(shell sysctl -n hw.ncpu)
else
  NPROC := $(shell nproc)
endif

.PHONY: build
build:
	jbuilder build @install -j $(NPROC) --dev

.PHONY: install
install: build
	jbuilder install

.PHONY: clean
clean:
	jbuilder clean

.PHONY: dep
dep:
	opam pin --no-action add ppx_deriving_yojson --dev-repo
	jbuilder external-lib-deps --missing --dev @install

.PHONY: test
test: build
	jbuilder runtest --dev
