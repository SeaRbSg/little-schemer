files = $(wildcard ch*.rkt)
targets = $(patsubst %.rkt,%,$(files))

test:
	@time raco test --direct $(files)

.PHONY: $(targets) all

$(targets):
	@time raco test $@.rkt

all: $(targets)
