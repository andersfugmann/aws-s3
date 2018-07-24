.PHONY: build
build:
	dune build @install

.PHONY: install
install: build
	dune install

.PHONY: clean
clean:
	dune clean

.PHONY: test
test: build
	dune runtest

update-version: VERSION=$(shell cat Changelog | grep -E '^[0-9]' | head -n 1 | cut -f1 -d':' )
update-version:
	@echo "Set version to $(VERSION)"
	@sed -i 's/^version: .*/version: "$(VERSION)"/' *.opam
	@sed -i 's/"\(aws-s3[-a-z]*\)"[ ]*{ = .* }/"\1" { = "$(VERSION)" }/' *.opam

release: VERSION=$(shell cat Changelog | grep -E '^[0-9]' | head -n 1 | cut -f1 -d':')
release:
	@./release.sh $(VERSION)

doc:
	dune build --dev @doc

gh-pages: doc
	git clone `git config --get remote.origin.url` .gh-pages --reference .
	git -C .gh-pages checkout --orphan gh-pages
	git -C .gh-pages reset
	git -C .gh-pages clean -dxf
	cp  -r _build/default/_doc/_html/* .gh-pages
	git -C .gh-pages add .
	git -C .gh-pages config user.email 'docs@aws-s3'
	git -C .gh-pages commit -m "Update documentation"
	git -C .gh-pages push origin gh-pages -f
	rm -rf .gh-pages
