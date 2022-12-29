export GOBIN = $(HOME)/lib/gotools

all: $(GOBIN)
	egrep '^\s+_' tools.go  | awk '{print $$2}' | xargs go install

$(GOBIN):
	mkdir -p $(GOBIN)
