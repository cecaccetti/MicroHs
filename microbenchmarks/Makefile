# Find all .hs files in current directory
HS_FILES := $(wildcard *.hs)
# Generate list of target .comb files
COMB_FILES := $(HS_FILES:.hs=.comb)
ROM_FILES := $(HS_FILES:.hs=.scala)
# Set Chisel project path
MUTATOR_PATH := ~/workspace/sc-machine/src/main/scala/benchmarks

# Rule to process all .hs files
all: $(ROM_FILES)

# Rule to handle single argument
%:
	mhs -i../lib $@ -o$@.comb
	mhs -i../lib $@ -o$@.scala

# Pattern rule to convert .hs to .scala ROM
%.scala: %.hs
	mhs -i../lib $* -o$@

# Pattern rule to convert .hs to .comb
%.comb: %.hs
	mhs -i../lib $* -o$@

# Clean rule to remove generated files
clean:
	rm -f *.comb *.scala

# Rule to clean and reprocess all files
rerun: clean all

# Rule for installing binary to interpreter project
install:
	cp *.scala $(MUTATOR_PATH)/

.PHONY: help clean all rerun % install
