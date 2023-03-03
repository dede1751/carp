EXE     = carp
ifeq ($(OS),Windows_NT)
NAME := $(EXE).exe
else
NAME := $(EXE)
endif

rule:
	cargo rustc --release -p carp -- -C target-cpu=native --emit link=$(NAME)