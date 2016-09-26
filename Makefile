ts_dns:
	erlc -I include src/tsung/ts_dns.erl
ts_dns_controller:
	erlc -I include src/tsung_controller/ts_config_dns.erl

all: ts_dns ts_dns_controller

install:
	cp ts_config_dns.beam /usr/lib/erlang/lib/tsung_controller-1.6.0/ebin/
	cp ts_dns.beam /usr/lib/erlang/lib/tsung-1.6.0/ebin/
	cp tsung-1.0.dtd /usr/share/tsung/
