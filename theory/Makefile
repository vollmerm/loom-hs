AGDA ?= agda
MAIN := src/Loom/Theory/Main.agda

.PHONY: all check compile-traversal-export

all: check

check:
	$(AGDA) $(MAIN)

compile-traversal-export:
	$(AGDA) --compile --no-main src/Loom/Theory/Traversal/Export.agda
