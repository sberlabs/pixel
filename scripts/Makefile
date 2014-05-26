BIN=../bin
J2=../j2

all: $(BIN)/monitor $(J2)/monitor.j2 $(BIN)/tidyup

$(BIN)/tidyup: tidyup.erl
	cp tidyup.erl $(BIN)/tidyup
	chmod a+x $(BIN)/tidyup

$(BIN)/monitor: monitor_dev.hdr monitor.erl
	cat monitor_dev.hdr monitor.erl  >$(BIN)/monitor
	chmod a+x $(BIN)/monitor

$(J2)/monitor.j2: monitor_prod.hdr monitor.erl
	cat monitor_prod.hdr monitor.erl  >$(J2)/monitor.j2

clean:
	-rm $(BIN)/monitor
	-rm $(J2)/monitor.j2

.PHONY: clean all

.DEFAULT_GOAL: all
