# Code Maintenance
- [ ] Test suite
  - [ ] Roundtrip test
  - [ ] Figure out how to test for memory leaks
- [ ] Better support for error handling
# Documentation
- [ ] Note that we don't support deprecated functions in the documentation
- [ ] Add example in haddocks
# Features
- [ ] `pipes` support?
- [ ] check for tarbombs?
- [ ] pack a `[Entry]` to archive and return bytestring
- [ ] Write archives besides just tarballs (e.g. zipfiles?)
# Performance
- [ ] Lots of the memory stuff is inefficient as-is
  - [ ] We should at least support streaming when unpacking (?)
- [ ] compare to `tar` library
