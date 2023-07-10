# Build executables for Carp releases. Base rule is reserved for OpenBench
EXE   := Carp
LXE   := carp
_THIS := $(realpath $(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
TMPDIR := $(_THIS)/tmp

ifeq ($(OS),Windows_NT)
	NAME := $(EXE).exe
	PGO  := $(EXE)-pgo.exe
	V1NAME := $(LXE)-x86_64-win-v1.exe
	V2NAME := $(LXE)-x86_64-win-v2.exe
	V3NAME := $(LXE)-x86_64-win-v3.exe
	V4NAME := $(LXE)-x86_64-win-v4.exe
else
	NAME := $(EXE)
	PGO  := $(EXE)-pgo.exe
	V1NAME := $(LXE)-x86_64-linux-v1
	V2NAME := $(LXE)-x86_64-linux-v2
	V3NAME := $(LXE)-x86_64-linux-v3
	V4NAME := $(LXE)-x86_64-linux-v4
endif

rule:
	cargo rustc --release -- -C target-cpu=native --emit link=$(NAME)

pgo:
	rm -rf $(TMPDIR)
	RUSTFLAGS="-Cprofile-generate=$(TMPDIR)" \
		cargo rustc --release -- -C target-cpu=native --emit link=$(PGO)
	
	./$(PGO) bench

	llvm-profdata merge -o $(TMPDIR)/merged.profdata $(TMPDIR)

	RUSTFLAGS="-Cprofile-use=$(TMPDIR)/merged.profdata" \
		cargo rustc --release -- -C target-cpu=native --emit link=$(NAME)

	rm -rf $(TMPDIR)
	rm $(PGO)
	rm *.pdb

release:
	cargo rustc --release -- -C target-cpu=x86-64 --emit link=$(V1NAME)
	cargo rustc --release -- -C target-cpu=x86-64-v2 --emit link=$(V2NAME)
	cargo rustc --release -- -C target-cpu=x86-64-v3 --emit link=$(V3NAME)
	cargo rustc --release -- -C target-cpu=x86-64-v4 --emit link=$(V4NAME)