# Build executables for Carp releases. Base rule is reserved for OpenBench
EXE   := Carp
LXE   := carp
_THIS := $(realpath $(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
TMP := $(_THIS)/tmp

ifeq ($(OS),Windows_NT)
	EXT := .exe
	VER := win
	PROF := llvm-profdata
else ifeq ($(shell uname -s), Darwin)
	EXT :=
	VER := darwin
	PROF := xcrun llvm-profdata
else
	EXT :=
	VER := linux
	PROF := llvm-profdata
endif
NAME := $(EXE)$(EXT)

.PHONY: rule tmp-dir release syzygy datagen

rule:
	cargo rustc -r -p engine --bins -- -C target-cpu=native --emit link=$(NAME)

# $(call DO_PGO, pkg, features, target_cpu, outname, run_cmd)
define DO_PGO
	cargo rustc -r -p $(1) \
		$(if $(2),--features $(2),) -- \
		-C target-feature=+crt-static -C target-cpu=$(3) \
		-C profile-generate=$(TMP) \
		--emit link=pgo
	$(5)
	${PROF} merge -o $(TMP)/merged.profdata $(TMP)
	cargo rustc -r -p $(1) \
		$(if $(2),--features $(2),) -- \
		-C target-feature=+crt-static -C target-cpu=$(3) \
		-C profile-use=$(TMP)/merged.profdata \
		--emit link=$(4)

	rm -rf $(TMP)/*
	rm -f *.pdb
	rm pgo
endef

tmp-dir:
	mkdir -p $(TMP)

x86-64-v1 x86-64-v2 x86-64-v3 x86-64-v4 apple-m1 apple-m2 apple-m3 apple-m4 generic native: tmp-dir
	$(call DO_PGO,engine --bins,,${@},$(LXE)-$(VER)-$@$(EXT),./pgo bench 16)

syzygy: tmp-dir
	$(call DO_PGO,engine --bins,syzygy,native,$(LXE)-$(VER)-syzygy$(EXT),./pgo bench 16)

datagen: tmp-dir
	$(call DO_PGO,tools,,native,datagen$(EXT),./pgo datagen -g 256 -t 32 -n 5000 ; ./pgo datagen -g 256 -t 32 -d 8)
	rm -rf $(_THIS)/data

trainer:
	cargo rustc -r -p tools --features train -- -C target-cpu=native --emit link=trainer${EXT}

release-x86: x86-64-v1 x86-64-v2 x86-64-v3 x86-64-v4
