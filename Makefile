export IDRIS2 ?= idris2

lib_pkg = pq.ipkg

.PHONY: all lib install install-with-src clean clean-install develop

all: lib

clean-install: clean install

lib:
	${IDRIS2} --build ${lib_pkg}

install:
	${IDRIS2} --install ${lib_pkg}

install-with-src:
	${IDRIS2} --install-with-src ${lib_pkg}

clean:
	${IDRIS2} --clean ${lib_pkg}
	${RM} -r build

develop:
	find -name "*.idr" | entr -d idris2 --typecheck ${lib_pkg}
