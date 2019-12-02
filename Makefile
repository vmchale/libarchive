.PHONY: clean

clean:
	rm -rf dist-newstyle dist test/data/*.tar* *.hp *.prof *.chi *.chs.h stack.yaml.lock .hspec-failures
	touch .hspec-failures
