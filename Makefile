FPC = fpc

.PHONY: all clean

all: bin/bindgen

bin/bindgen: src/*.pas
	$(FPC) -Mobjfpc -Sh -Fusrc -FUobj -FEbin src/bindgen.pas

clean:
	rm -f obj/* bin/*
