files = $(wildcard ch*.rkt)
targets = $(patsubst %.rkt,%,$(files))

test:
	@time raco test --direct $(files)

cover:
	@time raco cover $(files)

clean:
	rm -Rf coverage

.PHONY: $(targets) all

$(targets):
	@time raco test $@.rkt

all: $(targets)
