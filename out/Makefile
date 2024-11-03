# Find all .hs files in current directory
HS_FILES := $(wildcard *.hs)
# Generate list of target .comb files
COMB_FILES := $(HS_FILES:.hs=.comb)

# Rule to process all .hs files
all: $(COMB_FILES)

# Rule to handle single argument
%:
	mhs -i../lib $@ -o$@.comb

# Pattern rule to convert .hs to .comb
%.comb: %.hs
	mhs -i../lib $* -o$@

# Clean rule to remove generated files
clean:
	rm -f *.comb
	
# Rule to clean and reprocess all files
rerun: clean all

.PHONY: help clean all rerun %
