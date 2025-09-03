# Build executables for Carp releases. Base rule is reserved for OpenBench
NAME := carp
_THIS := $(realpath $(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
TMP := $(_THIS)/tmp

ifeq ($(OS),Windows_NT)
	EXT := .exe
	VER := win

	PROF := llvm-profdata
	MKDIR := mkdir
    RMDIR := rmdir /s /q
	RMFILE := del
else ifeq ($(shell uname -s), Darwin)
	EXT :=
	VER := darwin

	PROF := xcrun llvm-profdata
	MKDIR := mkdir -p
    RMDIR := rm -rf
	RMFILE := rm -f
else
	EXT :=
	VER := linux

	PROF := llvm-profdata
	MKDIR := mkdir -p
    RMDIR := rm -rf
	RMFILE := rm -f
endif

# $(call DO_PGO, crate, features, target_cpu, target_features, outname, run_cmd)
define DO_PGO
	RUSTFLAGS="-C target-cpu=$(3)" cargo rustc -r -p $(1) $(if $(2),--features $(2),) -- $(if $(4),-C target-feature=$(4)) -C profile-generate=$(TMP) --emit link=pgo
	$(6)
	${PROF} merge -o $(TMP)/merged.profdata $(TMP)
	RUSTFLAGS="-C target-cpu=$(3)" cargo rustc -r -p $(1) $(if $(2),--features $(2),) -- $(if $(4),-C target-feature=$(4)) -C profile-use=$(TMP)/merged.profdata --emit link=$(5)

	$(RMDIR) $(TMP)/*
	$(RMFILE) *.pdb
	$(RMFILE) pgo
endef

###################################### OPENBENCH ##################################################

rule:
	RUSTFLAGS="-C target-cpu=native" cargo rustc -r -p engine --bins -- --emit link=Carp$(EXT)

################################### RELEASE BUILDS ################################################

x86-64-v1 apple-m1 apple-m2 apple-m3 apple-m4: tmp-dir
	$(call DO_PGO,engine --bins,syzygy,${@},+crt-static,$(NAME)-$(VER)-$@$(EXT),./pgo bench 16)

x86-64-v2 x86-64-v3: tmp-dir
	$(call DO_PGO,engine --bins,syzygy,${@},+crt-static,$(NAME)-$(VER)-$@$(EXT),./pgo bench 16)

x86-64-v4: tmp-dir
	$(call DO_PGO,engine --bins,syzygy,${@},+crt-static,$(NAME)-$(VER)-$@$(EXT),./pgo bench 16)

release-x86: x86-64-v1 x86-64-v2 x86-64-v3 x86-64-v4

##################################### DEV BUILDS ##################################################

bench:
	RUSTFLAGS="-C target-cpu=native" cargo r -r -p engine -- bench

native: tmp-dir
	$(call DO_PGO,engine --bins,syzygy,native,,$(NAME)-$(VER)-native$(EXT),./pgo bench 16)

datagen: tmp-dir
	$(call DO_PGO,tools,,native,,$(NAME)-datagen$(EXT),./pgo datagen -g 256 -t 32 -n 5000)
	$(RMDIR) $(_THIS)/data

trainer:
	RUSTFLAGS="-C target-cpu=native" cargo rustc -r -p tools --features train -- --emit link=$(NAME)-train$(EXT)

###################################################################################################

.PHONY: rule x86-64-v1 x86-64-v2 x86-64-v3 x86-64-v4 apple-m1 apple-m2 apple-m3 apple-m4 \
	release-x86 native syzygy datagen trainer tmp-dir

tmp-dir:
	$(MKDIR) -p $(TMP)
