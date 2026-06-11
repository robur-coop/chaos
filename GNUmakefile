vendors:
	test ! -d $@
	mkdir vendors
	@./source.sh

chaos.hvt.target: | vendors
	@echo " BUILD bin/main.exe"
	@dune build --root . --profile=release ./bin/main.exe
	@echo " DESCR bin/main.exe"
	@$(shell dune describe location \
		--context solo5 --no-print-directory --root . --display=quiet \
		./bin/main.exe 1> $@ 2>&1)

chaos.hvt: chaos.hvt.target
	@echo " COPY chaos.hvt"
	@cp $(file < chaos.hvt.target) $@
	@chmod +w $@
	@echo " STRIP chaos.hvt"
	@strip $@

chaos.install: chaos.hvt
	@echo " GEN chaos.install"
	@ocaml install.ml > $@

all: chaos.install | vendors

.PHONY: clean
clean:
	if [ -d vendors ] ; then rm -fr vendors ; fi
	rm -f chaos.hvt.target
	rm -f chaos.hvt
	rm -f chaos.install

install: chaos.intall
	@echo " INSTALL chaos"
	opam-installer chaos.install
