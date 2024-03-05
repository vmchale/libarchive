DOWNLOADER ?= wget
DOWNLOADER_OPTS ?= -O

.PHONY: clean

MAKEFLAGS += --warn-undefined-variables --no-builtin-rules -j
.DELETE_ON_ERROR:

setup: test/data/ghc-8.8.1-src.tar test/data/alsa-lib-1.1.9.tar test/data/llvm-9.0.0.src.tar test/data/ATS2-Postiats-0.3.13.tar test/data/libarchive-1.0.5.1.tar test/data/sparc64-linux-dist.tar test/data/ruby-3.0.0.tar test/data/mlton-20210117.src.tar test/data/wezterm-20240127-113634-bbcac864-src.tar

clean:
	rm -rf dist-newstyle \
	    test/data/*.tar.* \
	    test/data/*.tgz \
	    *.hp \
	    *.prof \
	    *.chi \
	    *.chs.h \
	    stack.yaml.lock \
	    .hspec-failures \
	    .stack-work \
	    tags \
	    *.svg \
	    test/data/alsa-lib-1.1.9.tar \
	    test/data/sparc64-linux-dist.tar \
	    test/data/ghc-8.8.1-src.tar \
	    test/data/ATS2-Postiats-0.3.13.tar \
	    test/data/libarchive-1.0.5.1.tar \
	    test/data/llvm-9.0.0.src.tar \
	    test/data/ruby-3.0.0.tar \
	    test/data/mlton-20210117.src.tar \
	    test/data/wezterm-20240127-113634-bbcac864-src.tar

packdeps.svg: libarchive.cabal
	cabal build --disable-benchmarks --disable-tests
	cabal-plan dot | dot -Tsvg -o $@

test/data:
	mkdir -p $@

test/data/wezterm-20240127-113634-bbcac864-src.tar: test/data/wezterm-20240127-113634-bbcac864-src.tar.gz
	gunzip -f $^

test/data/mlton-20210117.src.tar: test/data/mlton-20210117.src.tgz
	gunzip -f $^

test/data/ruby-3.0.0.tar: test/data/ruby-3.0.0.tar.gz
	gunzip -f $^

test/data/ghc-8.8.1-src.tar: test/data/ghc-8.8.1-src.tar.xz
	xz -d -f $^

test/data/alsa-lib-1.1.9.tar: test/data/alsa-lib-1.1.9.tar.bz2
	bzip2 -d -f $^

test/data/llvm-9.0.0.src.tar: test/data/llvm-9.0.0.src.tar.xz
	xz -d -f $^

test/data/sparc64-linux-dist.tar: test/data/sparc64-linux-dist.tar.gz
	gunzip -f $^

test/data/ATS2-Postiats-0.3.13.tar: test/data/ATS2-Postiats-0.3.13.tgz
	gunzip -f $^

test/data/libarchive-1.0.5.1.tar: test/data/libarchive-1.0.5.1.tar.gz
	gunzip -f $^

test/data/mlton-20210117.src.tgz: test/data
	$(DOWNLOADER) https://sourceforge.net/projects/mlton/files/mlton/20210117/mlton-20210117.src.tgz $(DOWNLOADER_OPTS) $@

test/data/ruby-3.0.0.tar.gz: test/data
	$(DOWNLOADER) https://cache.ruby-lang.org/pub/ruby/3.0/ruby-3.0.0.tar.gz $(DOWNLOADER_OPTS) $@

test/data/ghc-8.8.1-src.tar.xz: test/data
	$(DOWNLOADER) https://downloads.haskell.org/~ghc/8.8.1/ghc-8.8.1-src.tar.xz $(DOWNLOADER_OPTS) $@

test/data/alsa-lib-1.1.9.tar.bz2: test/data
	$(DOWNLOADER) https://www.alsa-project.org/files/pub/lib/alsa-lib-1.1.9.tar.bz2 $(DOWNLOADER_OPTS) $@

test/data/llvm-9.0.0.src.tar.xz: test/data
	$(DOWNLOADER) http://releases.llvm.org/9.0.0/llvm-9.0.0.src.tar.xz $(DOWNLOADER_OPTS) $@

test/data/ATS2-Postiats-0.3.13.tgz: test/data
	$(DOWNLOADER) http://ats-lang.sourceforge.net/IMPLEMENT/Postiats/ATS2-Postiats-0.3.13.tgz $(DOWNLOADER_OPTS) $@

test/data/libarchive-1.0.5.1.tar.gz: test/data
	$(DOWNLOADER) http://hackage.haskell.org/package/libarchive-1.0.5.1/libarchive-1.0.5.1.tar.gz $(DOWNLOADER_OPTS) $@

test/data/sparc64-linux-dist.tar.gz: test/data
	$(DOWNLOADER) https://github.com/vmchale/dickinson/releases/download/1.1.0.2/sparc64-linux-dist.tar.gz $(DOWNLOADER_OPTS) $@

test/data/wezterm-20240127-113634-bbcac864-src.tar.gz: test/data
	$(DOWNLOADER) https://github.com/wez/wezterm/releases/download/20240127-113634-bbcac864/wezterm-20240127-113634-bbcac864-src.tar.gz $(DOWNLOADER_OPTS) $@
