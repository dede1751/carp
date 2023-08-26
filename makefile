# Build executables for Carp releases. Base rule is reserved for OpenBench
EXE   := Carp
LXE   := carp
_THIS := $(realpath $(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
TMPDIR := $(_THIS)/carp-tmp

ifeq ($(OS),Windows_NT)
	EXT := .exe
	VER := win
	# Different native flag for macOS
else ifeq ($(shell uname -s), Darwin)
	EXT :=
	VER := darwin
else
	EXT :=
	VER := linux
endif

NAME := $(EXE)$(EXT)
PGO  := $(EXE)-pgo$(EXT)

rule:
	cargo rustc -r -p carp --bins -- -C target-cpu=native --emit link=$(NAME)

x86-64 x86-64-v2 x86-64-v3 x86-64-v4 native:
	rm -rf $(TMPDIR)
	RUSTFLAGS="$(RUSTFLAGS) -C profile-generate=$(TMPDIR)" \
		cargo rustc -r -p carp --bins -- -C target-cpu=$@ --emit link=$(PGO)
	
	./$(PGO) bench

	llvm-profdata merge -o $(TMPDIR)/merged.profdata $(TMPDIR)

	RUSTFLAGS="$(RUSTFLAGS) -C profile-use=$(TMPDIR)/merged.profdata" \
		cargo rustc -r -p carp --bins -- -C target-cpu=$@ --emit link=$(LXE)-$(VER)-$@$(EXT)

	rm -rf $(TMPDIR)
	rm $(PGO)

datagen:
	rm -rf $(TMPDIR)
	RUSTFLAGS="-C profile-generate=$(TMPDIR)" \
		cargo rustc -r -p carp-tools -- -C target-cpu=native --emit link=$(PGO)
	
	./$(PGO) datagen -g 256 -t 32 -n 5000
	./$(PGO) datagen -g 256 -t 32 -d 8

	llvm-profdata merge -o $(TMPDIR)/merged.profdata $(TMPDIR)

	RUSTFLAGS="-C profile-use=$(TMPDIR)/merged.profdata" \
		cargo rustc -r -p carp-tools -- -C target-cpu=native --emit link=datagen$(EXT)

	rm -rf $(TMPDIR)
	rm $(PGO)
	rm -rf $(_THIS)/data

release: x86-64 x86-64-v2 x86-64-v3 x86-64-v4
