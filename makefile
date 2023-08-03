# Build executables for Carp releases. Base rule is reserved for OpenBench
EXE   := Carp
LXE   := carp
_THIS := $(realpath $(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
TMPDIR := $(_THIS)/tmp

ifeq ($(OS),Windows_NT)
	EXT := .exe
	VER := win
else
	EXT :=
	VER := linux
endif

NAME := $(EXE)$(EXT)
PGO  := $(EXE)-pgo$(EXT)

rule:
	cargo rustc --release -- -C target-cpu=native --emit link=$(NAME)

x86-64 x86-64-v2 x86-64-v3 x86-64-v4 native:
	rm -rf $(TMPDIR)
	RUSTFLAGS="$(RUSTFLAGS) -C profile-generate=$(TMPDIR)" \
		cargo rustc --release -- -C target-cpu=$@ --emit link=$(PGO)
	
	./$(PGO) bench

	llvm-profdata merge -o $(TMPDIR)/merged.profdata $(TMPDIR)

	RUSTFLAGS="$(RUSTFLAGS) -C profile-use=$(TMPDIR)/merged.profdata" \
		cargo rustc --release -- -C target-cpu=$@ --emit link=$(LXE)-$(VER)-$@$(EXT)

	rm -rf $(TMPDIR)
	rm $(PGO)

datagen:
	rm -rf $(TMPDIR)
	RUSTFLAGS="-C profile-generate=$(TMPDIR)" \
		cargo rustc --features tools --release -- -C target-cpu=native --emit link=$(PGO)
	
	./$(PGO) --datagen '200g-32t-5000n'
	./$(PGO) --datagen '200g-32t-8d'

	llvm-profdata merge -o $(TMPDIR)/merged.profdata $(TMPDIR)

	RUSTFLAGS="-C profile-use=$(TMPDIR)/merged.profdata" \
		cargo rustc --features tools --release -- -C target-cpu=native --emit link=datagen$(EXT)

	rm -rf $(TMPDIR)
	rm $(PGO)
	rm -rf $(_THIS)/data

release: x86-64 x86-64-v2 x86-64-v3 x86-64-v4 native
