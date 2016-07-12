CPPFLAGS=-cc-options,-std=c++11
LIBS=-ldl -lcrypt

rserve.so: rserve.cpp
	swipl-ld $(CPPFLAGS) -shared -o $@ rserve.cpp cxx/Rconnection.o $(LIBS)

clean:
	rm -f rserve.so *~
