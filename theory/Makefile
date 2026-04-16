AGDA ?= agda
LIBRARIES_FILE := libraries
MAIN := src/Loom/Theory/Main.agda

.PHONY: all check compile-traversal-export

all: check

check:
	$(AGDA) --no-default-libraries --library-file=$(LIBRARIES_FILE) -l loom-hs-theory $(MAIN)

compile-traversal-export:
	$(AGDA) --no-default-libraries --library-file=$(LIBRARIES_FILE) -l loom-hs-theory --compile --no-main src/Loom/Theory/Traversal/Export.agda
