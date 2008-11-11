# This Makefile requires GNU Make
EMACS           = /usr/bin/env emacs
EMACS_OPTS      = --quick --batch -L lib -L spec -L vendor \
				  -l el-mock -l el-expectations
TEMPDIR         = /tmp
OUTPUT_FILE     = $(TEMPDIR)/el-expectations.out
SPEC_FILES      = org-rtm-spec

specs:
	- $(EMACS) $(EMACS_OPTS) -f batch-expectations $(OUTPUT_FILE) $(SPEC_FILES)
	- cat $(OUTPUT_FILE)
	- rm $(OUTPUT_FILE)
