CPPFLAGS=-std=c++11 -fPIC
SWICPPFLAGS=-cc-options,-std=c++11
COFLAGS=-O2 -gdwarf-2 -g3
LIBS=-ldl -lcrypt -lm
CXXCLIENT=Rserve/src/client/cxx
CXXDEPS= $(CXXCLIENT)/configure
RCONN=$(CXXCLIENT)/Rconnection.o
RSINCLUDE=-IRserve/src -IRserve/src/include -I$(CXXCLIENT)

rserve.so: rserve.cpp $(RCONN)
	swipl-ld $(SWICPPFLAGS) $(COFLAGS) $(RSINCLUDE) -shared -o $@ rserve.cpp $(RCONN) $(LIBS)

clean:
	rm -f rserve.so $(RCONN) *~

$(CXXCLIENT):
	git submodule update --init

$(CXXCLIENT)/configure: $(CXXCLIENT)
	cd $(CXXCLIENT) && autoheader && autoconf

$(CXXCLIENT)/Makefile: $(CXXCLIENT)/configure $(CXXCLIENT)/Makefile.in
	cd $(CXXCLIENT) && ./configure

$(RCONN): $(CXXCLIENT)/Makefile
	$(CXX) -c $(CPPFLAGS) $(COFLAGS) $(RSINCLUDE) -o $@ $(CXXCLIENT)/Rconnection.cc
