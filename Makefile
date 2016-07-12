CPPFLAGS=-cc-options,-std=c++11
COFLAGS=-gdwarf-2 -g3
LIBS=-ldl -lcrypt

rserve.so: rserve.cpp
	swipl-ld $(CPPFLAGS) $(COFLAGS) -shared -o $@ rserve.cpp cxx/Rconnection.o $(LIBS)

clean:
	rm -f rserve.so *~
