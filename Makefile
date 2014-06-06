VARS_A="node_name=pixel_a http_port=8080 profiler=false udp_host=localhost udp_port=6363"
VARS_B="node_name=pixel_b http_port=8081 profiler=false udp_host=localhost udp_port=6363"

all: src/*.erl
	rebar compile

deps:
	rebar get-deps

clean:
	rm -fr deps ebin
	mkdir deps ebin

deploy_a:
	rebar get-deps
	rebar compile
	rm -fr _rel
	./relx release tar
	cd playbooks/deploy
	ansible-playbook deploy.yml -i ../hosts -l prod --extra-vars $(VARS_A)

deploy_b:
	rebar get-deps
	rebar compile
	rm -fr _rel
	./relx release tar
	cd playbooks/deploy
	ansible-playbook deploy.yml -i ../hosts -l prod --extra-vars $(VARS_B)

.PHONY: all clean deps deploy_a deploy_b
