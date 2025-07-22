# Variables
ERLC=erlc
SRC=$(wildcard *.erl)
BEAM=$(SRC:.erl=.beam)

# Compilar todos los archivos
all: p2p_utils.beam p2p_udp.beam p2p_node.beam

p2p_utils.beam: p2p_utils.erl
	$(ERLC) p2p_utils.erl

p2p_udp.beam: p2p_udp.erl p2p_utils.beam
	$(ERLC) p2p_udp.erl

p2p_node.beam: p2p_node.erl p2p_udp.beam p2p_utils.beam
	$(ERLC) p2p_node.erl

# Limpiar archivos compilados
clean:
	rm -f *.beam
