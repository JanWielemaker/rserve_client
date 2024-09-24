# Build rserve_client for SWi-Prolog

CPPFLAGS=-std=c++17 -fPIC
SOBJ=   $(SWIPL_MODULE_DIR)/rserve.$(SWIPL_MODULE_EXT)
SWIPL_CFLAGS+=-I$(HDTHOME)/include
SWICPPFLAGS=-std=c++17
COFLAGS=-O2 -gdwarf-2 -g3
LIBS=-ldl -lcrypt -lm

# The following variables should be set by Make, but in case they're
# not, get the values that swipl sets (also in buildenv.sh)
CC?=$(SWIPL_CC)
CXX?=$(SWIPL_CXX)

# The following should be set by buildenv.sh:
SWIPL?=swipl

CXXCLIENT=Rserve/src/client/cxx
CXXDEPS= $(CXXCLIENT)/configure
RCONN=$(CXXCLIENT)/Rconnection.o
RCONNH=$(CXXCLIENT)/Rconnection.h
RCONNIN=$(CXXCLIENT)/Rconnection.cc $(RCONNH)
RSINCLUDE=-IRserve/src -IRserve/src/include -I$(CXXCLIENT)

OBJ=cc/rserve.o $(RCONN)

all:	$(SOBJ)

$(SOBJ): $(OBJ)
	mkdir -p $(SWIPL_MODULE_DIR)
	$(CXX) $(SWIPL_MODULE_LDFLAGS) -o $@ $(OBJ) $(LIBS) $(SWIPL_MODULE_LIB)

cc/rserve.o: cc/rserve.cc $(CXXCLIENT)/Makefile $(RCONNH)
	$(CXX) $(SWIPL_CFLAGS) $(COFLAGS) $(SWICPPFLAGS) $(RSINCLUDE) -c -o $@ $<

clean:
	$(RM) cc/rserve.o $(RCONN) *~

distclean: clean
	$(RM) $(SOBJ) status.db buildenv.sh

check::
install::

$(CXXCLIENT)/configure.ac:
	@if [ -d .git ]; then \
	  git submodule update --init; \
	else \
	  git clone -b janw https://github.com/JanWielemaker/Rserve; \
	fi

$(CXXCLIENT)/configure: $(CXXCLIENT)/configure.ac
	cd $(CXXCLIENT) && autoheader && autoconf

$(CXXCLIENT)/Makefile: $(CXXCLIENT)/configure $(CXXCLIENT)/Makefile.in
	cd $(CXXCLIENT) && ./configure

$(RCONN): $(CXXCLIENT)/Makefile $(RCONNIN)
	$(CXX) -c $(CPPFLAGS) $(COFLAGS) $(RSINCLUDE) -o $@ $(CXXCLIENT)/Rconnection.cc
