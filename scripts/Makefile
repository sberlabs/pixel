BIN=../bin
J2=../j2

all: $(BIN)/log_eater $(BIN)/monitor $(J2)/log_eater.j2 $(J2)/monitor.j2

$(BIN)/log_eater: log_eater_dev.hdr log_eater.erl
	cat log_eater_dev.hdr log_eater.erl  >$(BIN)/log_eater
	chmod a+x $(BIN)/log_eater

$(BIN)/monitor: monitor_dev.hdr monitor.erl
	cat monitor_dev.hdr monitor.erl  >$(BIN)/monitor
	chmod a+x $(BIN)/monitor

$(J2)/log_eater.j2: log_eater_prod.hdr log_eater.erl
	cat log_eater_prod.hdr log_eater.erl  >$(J2)/log_eater.j2

$(J2)/monitor.j2: monitor_prod.hdr monitor.erl
	cat monitor_prod.hdr monitor.erl  >$(J2)/monitor.j2

clean:
	-rm $(BIN)/log_eater 
	-rm $(BIN)/monitor
	-rm $(J2)/log_eater.j2
	-rm $(J2)/monitor.j2

.PHONY: clean all

.DEFAULT_GOAL: all
