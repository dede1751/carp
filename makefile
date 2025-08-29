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

# Detect native SIMD features for x86-64
SIMD_FEATURES := $(shell \
    cpuinfo=$$(grep -m1 '^flags' /proc/cpuinfo); \
    features=""; \
    echo "$$cpuinfo" | grep -qw "avx2"        && features="$$features,+avx2"; \
    echo "$$cpuinfo" | grep -qw "avxvnni"     && features="$$features,+avxvnni"; \
    echo "$$cpuinfo" | grep -qw "avx512f"     && features="$$features,+avx512f"; \
    echo "$$cpuinfo" | grep -qw "avx512vnni"  && features="$$features,+avx512vnni"; \
    echo $${features#,} \
)

# $(call DO_PGO, pkg, features, target_cpu, target_features, outname, run_cmd)
define DO_PGO
	RUSTFLAGS="-C target-cpu=$(3) $(if $(4),-C target-feature=$(4)) -C profile-generate=$(TMP)" \
		cargo rustc -r -p $(1) $(if $(2),--features $(2),) -- --emit link=pgo

	$(6)

	${PROF} merge -o $(TMP)/merged.profdata $(TMP)
	RUSTFLAGS="-C target-cpu=$(3) $(if $(4),-C target-feature=$(4)) -C profile-use=$(TMP)/merged.profdata" \
		cargo rustc -r -p $(1) $(if $(2),--features $(2),) -- --emit link=$(5)

	rm -rf $(TMP)/*
	rm -f *.pdb
	rm pgo
endef

###################################### OPENBENCH ##################################################

rule:
	cargo clean
	cargo rustc -r -p engine --bins -- -C target-cpu=native --emit link=$(NAME)

################################### RELEASE BUILDS ################################################

x86-64-v1 apple-m1 apple-m2 apple-m3 apple-m4: tmp-dir
	$(call DO_PGO,engine --bins,,${@},"+crt-static",$(LXE)-$(VER)-$@$(EXT),./pgo bench 16)

x86-64-v2 x86-64-v3: tmp-dir
	$(call DO_PGO,engine --bins,,${@},"+avx2,+crt-static",$(LXE)-$(VER)-$@$(EXT),./pgo bench 16)

x86-64-v4: tmp-dir
	$(call DO_PGO,engine --bins,,${@},"+avx512f,+crt-static",$(LXE)-$(VER)-$@$(EXT),./pgo bench 16)

release-x86: x86-64-v1 x86-64-v2 x86-64-v3 x86-64-v4

##################################### DEV BUILDS ##################################################

native: tmp-dir
	$(call DO_PGO,engine --bins,,native,${SIMD_FEATURES},$(LXE)-$(VER)-native$(EXT),./pgo bench 16)

syzygy: tmp-dir
	$(call DO_PGO,engine --bins,syzygy,native,${SIMD_FEATURES},$(LXE)-$(VER)-syzygy$(EXT),./pgo bench 16)

datagen: tmp-dir
	$(call DO_PGO,tools,,native,${SIMD_FEATURES},datagen$(EXT),./pgo datagen -g 256 -t 32 -n 5000)
	rm -rf $(_THIS)/data

trainer:
	cargo rustc -r -p tools --features train -- \
		-C target-cpu=native \
		$(if $(SIMD_FEATURES),-C target-feature=$(SIMD_FEATURES)) \
		--emit link=trainer${EXT}

###################################################################################################

.PHONY: rule x86-64-v1 x86-64-v2 x86-64-v3 x86-64-v4 apple-m1 apple-m2 apple-m3 apple-m4 \
	release-x86 native syzygy datagen trainer tmp-dir

tmp-dir:
	mkdir -p $(TMP)
