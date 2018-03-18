.PHONY: build
build:
	jbuilder build @install --dev

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

bump_version:
	@if [ -z "$(VERSION)" ]; then echo "need to set VERSION"; exit 1; fi
	@sed -i 's/^version: .*/version: "$(VERSION)"/' *.opam
	@sed -i 's/"\(aws-s3[-a-z]*\)"[ ]*{ = .* }/"\1" { = "$(VERSION)" }/' *.opam
	@echo ok: $(VERSION).
