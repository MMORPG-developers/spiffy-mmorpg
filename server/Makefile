all: existentialist_server.beam

clean:
	rm *.beam

.phony: all clean

%.beam: %.erl
	erlc $<

existentialist_server.beam: array_2d.beam client_connection.beam

