#@BEGIN-C_SRC-DEFAULT-RULES@
ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
OS=$(shell uname -s)
CFLAGS=-I$(ERL_TOP)/usr/include
#@END-C_SRC-DEFAULT-RULES@

CFLAGS+=-std=c99 -Wall -Werror -O
OCFLAGS=-I/usr/local/lib/erlang/usr/include -I/usr/include/SDL2 -D_REENTRANT
SOLDFLAGS=-shared
S2DLDFLAGS=-L/usr/lib/x86_64-linux-gnu -L/usr/local/lib -lsimple2d -lSDL2 -lGL -lm -lSDL2_image -lSDL2_mixer -lSDL2_ttf

all: ../priv/simulator_nif.so

../priv/simulator_nif.so: simulator_nif.o simulator_lib.o
	$(CC) $(ERL_TOP)/usr/lib/libei.a simulator_nif.o simulator_lib.o $(SOLDFLAGS) $(S2DLDFLAGS) -o ../priv/simulator_nif.so

simulator_lib.o: simulator_lib.c simulator_lib.h
	$(CC) $(OCFLAGS) -fPIC -c simulator_lib.c

simulator_nif.o: simulator_nif.c simulator_lib.h
	$(CC) $(OCFLAGS) -fPIC -c simulator_nif.c

clean:
	rm -f *.o ../priv/simulator_nif.so
