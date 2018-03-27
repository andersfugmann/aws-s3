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

update-version: VERSION=$(shell cat Changelog | grep -E '^[0-9]' | head -n 1 | cut -f1 -d':' )
update-version:
	@echo "Set version to $(VERSION)"
	@sed -i 's/^version: .*/version: "$(VERSION)"/' *.opam
	@sed -i 's/"\(aws-s3[-a-z]*\)"[ ]*{ = .* }/"\1" { = "$(VERSION)" }/' *.opam

release: VERSION=$(shell cat Changelog | grep -E '^[0-9]' | head -n 1 | cut -f1 -d':')
release:
	@./release.sh $(VERSION)
