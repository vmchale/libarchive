.PHONY: clean

MAKEFLAGS += --warn-undefined-variables --no-builtin-rules -j
.DELETE_ON_ERROR:

setup: test/data/ghc-8.8.1-src.tar test/data/alsa-lib-1.1.9.tar test/data/llvm-9.0.0.src.tar test/data/ATS2-Postiats-0.3.13.tar test/data/libarchive-1.0.5.1.tar test/data/sparc64-linux-dist.tar test/data/ruby-3.0.0.tar test/data/mlton-20210117.src.tar

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
	    test/data/mlton-20210117.src.tar

packdeps.svg: libarchive.cabal
	cabal build --disable-benchmarks --disable-tests
	cabal-plan dot | dot -Tsvg -o $@

test/data:
	mkdir -p $@

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
	wget https://sourceforge.net/projects/mlton/files/mlton/20210117/mlton-20210117.src.tgz -O $@

test/data/ruby-3.0.0.tar.gz: test/data
	wget https://cache.ruby-lang.org/pub/ruby/3.0/ruby-3.0.0.tar.gz -O $@

test/data/ghc-8.8.1-src.tar.xz: test/data
	wget https://downloads.haskell.org/~ghc/8.8.1/ghc-8.8.1-src.tar.xz --no-check-certificate -O $@

test/data/alsa-lib-1.1.9.tar.bz2: test/data
	wget https://www.alsa-project.org/files/pub/lib/alsa-lib-1.1.9.tar.bz2 --no-check-certificate -O $@

test/data/llvm-9.0.0.src.tar.xz: test/data
	wget http://releases.llvm.org/9.0.0/llvm-9.0.0.src.tar.xz --no-check-certificate -O $@

test/data/ATS2-Postiats-0.3.13.tgz: test/data
	wget http://ats-lang.sourceforge.net/IMPLEMENT/Postiats/ATS2-Postiats-0.3.13.tgz --no-check-certificate -O $@

test/data/libarchive-1.0.5.1.tar.gz: test/data
	wget http://hackage.haskell.org/package/libarchive-1.0.5.1/libarchive-1.0.5.1.tar.gz --no-check-certificate -O $@

test/data/sparc64-linux-dist.tar.gz: test/data
	wget https://github.com/vmchale/dickinson/releases/download/1.1.0.2/sparc64-linux-dist.tar.gz -O $@
