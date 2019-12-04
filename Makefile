.PHONY: build
build:
	dune build @install

.PHONY: lwt
lwt:
	dune build aws-s3-lwt.install

.PHONY: async
async:
	dune build aws-s3-async.install

.PHONY: install
install: build
	dune install

.PHONY: clean
clean:
	dune clean

.PHONY: test
test: build
	dune runtest

.PHONY: integration
integration:
	./integration.sh

release:
	opam publish

doc:
	dune build @doc

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
