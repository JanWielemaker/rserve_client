/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016, CWI Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#define MAIN	     // we want build_sin()
#define SOCK_ERRORS  // we will use verbose socket errors
#define PL_ARITY_AS_SIZE

#include <iostream>
#include <math.h>
#include <assert.h>
#include <mutex>
#include <vector>
#include <sisocks.h>
#include <Rconnection.h>
#include <SWI-Stream.h>
#include <SWI-cpp2.h>

		 /*******************************
		 *	       SYMBOL		*
		 *******************************/

#define R_DESTROYED	0x0001		/* Was destroyed by user  */
#define R_OPEN_ONCE	0x0002		/* Reuse alias */

typedef struct Rref
{ Rconnection   *rc;			/* Connection handle */
  PlAtom         symbol;		/* associated symbol */
  PlAtom	 name;			/* alias name */
  int	         flags;			/* flags */
} Rref;


		 /*******************************
		 *	      ALIAS		*
		 *******************************/

typedef struct alias_cell
{ PlAtom	name;
  PlAtom	symbol;
  struct alias_cell *next;
} alias_cell;

#define ALIAS_HASH_SIZE 64

std::mutex alias_lock;
static unsigned int alias_size = ALIAS_HASH_SIZE;
static alias_cell *alias_entries[ALIAS_HASH_SIZE];

// TODO: use AtomMap in SWI-cpp2-atommap.h (which also does locking)

static unsigned int
atom_hash(PlAtom a)
{ return (unsigned int)(a.unwrap()>>7) % alias_size;
}

static PlAtom
get_alias(PlAtom name)
{ for(alias_cell *c = alias_entries[atom_hash(name)];
      c;
      c = c->next)
  { if ( c->name == name )
      return c->symbol;
  }

  return PlAtom(PlAtom::null);
}

static void
alias(PlAtom name, PlAtom symbol)
{ unsigned int key = atom_hash(name);

  alias_lock.lock();
  if ( get_alias(name).is_null() )
  { alias_cell *c = (alias_cell *)malloc(sizeof *c);

    c->name   = name;
    c->symbol = symbol;
    c->next   = alias_entries[key];
    alias_entries[key] = c;
    c->name.register_ref();
    c->symbol.register_ref();
    alias_lock.unlock();
  } else
  { alias_lock.unlock();
    throw PlPermissionError("alias", "rserve", PlTerm(name));
  }
}

static void
unalias(PlAtom name)
{ unsigned int key = atom_hash(name);
  alias_cell *c, *prev=nullptr;

  alias_lock.lock();
  for(c = alias_entries[key]; c; prev=c, c = c->next)
  { if ( c->name == name )
    { if ( prev )
	prev->next = c->next;
      else
	alias_entries[key] = c->next;
      c->name.unregister_ref();
      c->symbol.unregister_ref();
      free(c);

      break;
    }
  }
  alias_lock.unlock();
}


		 /*******************************
		 *	 SYMBOL REFERENCES	*
		 *******************************/

static int
write_R_ref(IOSTREAM *s, atom_t symbol, int flags)
{ Rref **refp = (Rref **)Plx_blob_data(symbol, nullptr, nullptr);
  Rref *ref = *refp;
  (void)flags;

  Sfprintf(s, "<Rserve>(%p)", ref);
  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
GC an rserve connection from the atom garbage collector.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
release_R_ref(atom_t symbol)
{ Rref **refp = (Rref **)Plx_blob_data(symbol, nullptr, nullptr);
  Rref *ref   = *refp;
  Rconnection *rc;

  assert(ref->name.is_null());

  if ( (rc=ref->rc) )
  { ref->rc = nullptr;
    delete rc;
  }
  Plx_free(ref);

  return TRUE;
}


static int
save_R_ref(atom_t symbol, IOSTREAM *fd)
{ Rref **refp = (Rref **)Plx_blob_data(symbol, nullptr, nullptr);
  Rref *ref   = *refp;
  (void)fd;

  return PL_warning("Cannot save reference to <Rserve>(%p)", ref);
}


static atom_t
load_R_ref(IOSTREAM *fd)
{ (void)fd;

  return Plx_new_atom("<saved-Rserve-ref>");
}


// TODO: use the new (SWI-cpp2.h) blob interface

static PL_blob_t R_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE,
  (char*)"rserve",
  release_R_ref,
  nullptr,
  write_R_ref,
  nullptr,
  save_R_ref,
  load_R_ref
};


static int
unify_R_ref(PlTerm t, Rref *ref)
{ if ( ref->name.not_null() )
  { if ( ref->symbol.is_null() )
    { PlTermScoped tmp;

      if ( tmp.unify_blob(&ref, sizeof ref, &R_blob) &&
	   tmp.get_atom(&ref->symbol) )
      { alias(ref->name, ref->symbol);
      } else
      { assert(0);
      }
    }
    return t.unify_atom(ref->name);
  } else if ( ref->symbol.not_null() )
  { return t.unify_atom(ref->symbol);
  } else
  { return ( t.unify_blob(&ref, sizeof ref, &R_blob) &&
	     t.get_atom(&ref->symbol)
	   );
  }
}


static Rref*
symbol_Rref(PlAtom symbol)
{ void *data;
  size_t len;
  PL_blob_t *type;

  if ( (data=symbol.blob_data(&len, &type)) && type == &R_blob )
  { Rref **erd = (Rref **)data;
    return *erd;
  }

  return (Rref*)nullptr;
}


static int
get_Rref(PlTerm t, Rref **erp, int warn=TRUE)
{ PlAtom a(PlAtom::null);

  if ( t.get_atom(&a) )
  { for(int i=0; a.not_null() && i<2; i++)
    { Rref *ref;

      if ( (ref=symbol_Rref(a)) )
      { if ( !(ref->flags & R_DESTROYED) )
	{ *erp = ref;
	  return TRUE;
	} else if ( warn )
	{ throw PlExistenceError("Rserve", t);
	}
      }

      a = get_alias(a);
      if ( a.is_null() )
      { PlTerm_var r;
	PlTermv av(PlTerm(t), r);

	if ( !PlCall("rserve", "r_open_hook", av) ||
	     !r.get_atom(&a) )
	  break;
      }
    }

    throw PlExistenceError("Rserve", t);
  }

  if ( warn )
    throw PlTypeError("Rserve", t);

  return FALSE;
}

		 /*******************************
		 *	      UTIL		*
		 *******************************/

static const std::string
sisocks_msg(int rc)
{ char msg[128];

  sockerrorchecks(msg, sizeof msg, -1);

  return msg;
}

class SISocksError : public PlException
{
public:
  SISocksError(int rc) : // TODO: use PlGeneralError()
    PlException(PlCompound("error",
			   PlTermv(PlCompound("sisocks_error",
					      PlTermv(PlTerm_integer(rc),
						      PlTerm_string(sisocks_msg(rc)))),
				   PlTerm_var())))
  {
  }
};


static void
sisocks_ok(int status)
{ if ( status )
    throw SISocksError(status);
}


class RError : public PlException
{
public:
  RError(int status) : // TODO: use PlGeneralError()
    PlException(PlCompound("error",
			   PlTermv(PlCompound("r_error",
					      PlTermv(PlTerm_integer(status))),
				   PlTerm_var())))
  {
  }
};


static void
rok(int status)
{ if ( status )
    throw RError(status);
}


/* get_string() gets a C++ string from a Prolog term.  As R does not handle
 * strings with nul-bytes, we need to check for that.
 */

static void
get_string(PlTerm t, std::string *str)
{ *str = t.get_nchars(CVT_ATOM|CVT_STRING|CVT_NUMBER|CVT_EXCEPTION|REP_UTF8);
  if ( strlen(str->c_str()) != str->length() )
    throw PlDomainError("nul_terminated_string", t);
}


typedef enum dtype
{ D_UNKNOWN = 0,
  D_BOOLEAN,
  D_INTEGER,
  D_DOUBLE,
  D_STRING
} dtype;


static const PlAtom ATOM_false("false");
static const PlAtom ATOM_true("true");

class PlRExp
{
public:
  Rexp *exp = (Rexp*)nullptr;
  dtype type = D_UNKNOWN;
  std::vector<int> iv;
  std::vector<double> dv;
  std::string sv;

  PlRExp()
  {
  }

  ~PlRExp()
  { if ( exp )
      delete exp;
  }

  void
  promote(dtype t)
  { if ( t > type )
    { switch(t)
      { case D_BOOLEAN:
	  sv.reserve(16);
	  sv.append(sizeof (int), 0);
	  break;
	case D_INTEGER:
	  iv.reserve(16);
	  break;
        case D_DOUBLE:
	  dv.reserve(16);
	  if ( type == D_INTEGER )
	  { for(size_t i=0; i<iv.size(); i++)
	      dv.push_back((double)iv[i]);
	    iv.resize(0);
	  }
	  break;
	case D_STRING:
	  sv.reserve(16);
					/* FIXME: Promote integers and doubles */
	  break;
      }
      type = t;
    }
  }

  void append(PlTerm t)
  { switch(type)
    { case D_UNKNOWN:
	switch(t.type())
	{ case PL_VARIABLE:
	    throw PlInstantiationError(t);
	  case PL_INTEGER:
	    promote(D_INTEGER);
	    goto case_i;
	  case PL_FLOAT:
	    promote(D_DOUBLE);
	    goto case_d;
	  case PL_ATOM:
	  { PlAtom a(PlAtom::null);

	    if ( t.get_atom(&a) &&
		 (ATOM_true == a || ATOM_false == a) )
	    { promote(D_BOOLEAN);
	      goto case_b;
	    }
	  }
	  /*FALLTHROUGH*/
	  case PL_STRING:
	    promote(D_STRING);
	    goto case_s;
	  break;
	}
      case D_BOOLEAN:
      case_b:
      { int i;

	t.get_bool_ex(&i);
	sv.push_back(i != 0);
	break;
      }
      case D_INTEGER:
      case_i:
      { int i;
	double d;

	if ( t.get_integer(&i) )
	{ iv.push_back(i);
	} else if (t.get_float(&d) )
	{ promote(D_DOUBLE);
	  dv.push_back(d);
	} else
	  throw PlTypeError("numeric", t);
        break;
      }
      case D_DOUBLE:
      case_d:
	dv.push_back(t.unwrap()); // TODO(mgondan): is this correct?
        break;
      case D_STRING:
      case_s:
      { std::string s;
	get_string(t, &s);
	sv += s;
	sv.push_back(0);
	break;
      }
    }
  }

  void finish(PlTerm t)
  { switch(type)
    { case D_UNKNOWN:
	throw PlTypeError("R-expression", t);
      case D_BOOLEAN:
      { unsigned int *lenptr = (unsigned int*)sv.data();
	*lenptr = (unsigned int)(sv.size()-sizeof (unsigned int));
	exp = new Rboolean((unsigned char*)sv.data(), sv.size());
	break;
      }
      case D_INTEGER:
	exp = new Rinteger(iv.data(), iv.size());
	break;
      case D_DOUBLE:
	exp = new Rdouble(dv.data(), dv.size());
	break;
      case D_STRING:
      { sv.push_back(1);		/* s1\000s2\000...sn\000\001 */
	exp = new Rstrings(sv);
	break;
      }
    }
  }
};

static void
list_to_rexp(PlTerm t, PlRExp *exp)
{ PlTerm_tail list(t);
  PlTerm_var head;

  while( list.next(head) )
    exp->append(head);
  PlCheckFail(list.close());

  exp->finish(t);
}


static PlRExp *
term_to_rexp(PlTerm t)
{ PlRExp *exp = new PlRExp;

  switch(t.type())
  { case PL_LIST_PAIR:
    { list_to_rexp(t, exp);
      break;
    }
    case PL_INTEGER:
    { int i = t.as_int(); // TODO(mgondan): is this correct?
      exp->exp = new Rinteger(&i, 1);
      break;
    }
    case PL_FLOAT:
    { double f = t.as_double();
      exp->exp = new Rdouble(&f, 1);
      break;
    }
    case PL_ATOM:
    { PlAtom a(PlAtom::null);

      if ( t.get_atom(&a) &&
	   (ATOM_true == a || ATOM_false == a) )
      { struct { unsigned int len; unsigned char data[1]; } b;
	b.len = 1;
        b.data[0] = (ATOM_true == a);
	exp->exp = new Rboolean((unsigned char*)&b,
				sizeof(unsigned int)+sizeof(unsigned char));
	break;
      }
    }
    /*FALLTHROUGH*/
    case PL_STRING:
    { std::string s;

      get_string(t, &s);
      s.push_back(0);
      s.push_back(1);
      exp->exp = new Rstrings(s);
      break;
    }
    case PL_TERM:
    { std::string nm = t.name().as_string();
      if ( nm == "c" )
      { size_t len = t.arity();

	for(size_t i=1; i<= len; i++)
	  exp->append(t[i]);
	exp->finish(t);
	break;
      } else if ( nm == "+" && t.arity() == 1)
      { std::string s;

	get_string(t[1], &s);
	s.push_back(0);
	s.push_back(1);
	exp->exp = new Rstrings(s);
	break;
      }
    }
    /*FALLTHROUGH*/
    default:
      delete(exp);
      throw PlTypeError("R-expression", t);
  }

  return exp;
}


static const PlAtom ATOM_null("null");
static const PlAtom ATOM_clos("<XT_CLOS>");
static const PlAtom ATOM_unknown("<XT_UNKNOWN>");

static int
unify_exp(PlTerm t, const Rexp *exp)
{ switch(exp->type)
  { case XT_NULL:
      return t.unify_atom(ATOM_null);
    case XT_ARRAY_INT:
    { Rinteger *ri = (Rinteger*)exp;
      Rsize_t len = ri->length();
      PlTerm_tail tail(t);
      PlTerm_var h;

      for(Rsize_t i=0; i<len; i++)
      { h.put_integer(ri->intAt(i));
        if ( !tail.append(h) )
	  return FALSE;
      }
      return tail.close();
    }
    case XT_ARRAY_BOOL:
    { Rboolean *rs = (Rboolean*)exp;
      Rsize_t len = rs->length();
      PlTerm_tail tail(t);
      PlTerm_var h;

      for(Rsize_t i=0; i<len; i++)
      { h.put_bool(rs->boolAt(i));
        if ( !tail.append(h) )
	  return FALSE;
      }
      return tail.close();
    }
    case XT_ARRAY_DOUBLE:
    { Rdouble *rd = (Rdouble*)exp;
      Rsize_t len = rd->length();
      PlTerm_tail tail(t);
      PlTerm_var h;
      int allints = TRUE;

      for(Rsize_t i=0; i<len; i++)
      { double f = rd->doubleAt(i);
	if ( nearbyint(f) != f )
	{ allints = FALSE;
	  break;
	}
      }

      if ( allints )
      { for(Rsize_t i=0; i<len; i++)
        { h.put_int64((int64_t)rd->doubleAt(i));
          if ( !tail.append(h) )
	    return FALSE;
	}
      } else
      { for(Rsize_t i=0; i<len; i++)
        { h.put_float(rd->doubleAt(i));
            if ( !tail.append(h) )
	    return FALSE;
	}
      }
      return tail.close();
    }
    case XT_ARRAY_STR:
    { Rstrings *rs = (Rstrings*)exp;
      Rsize_t len = rs->length();
      PlTerm_tail tail(t);
      PlTerm_var h;

      for(Rsize_t i=0; i<len; i++)
      { h.put_variable();
        if ( !h.unify_chars(PL_STRING|REP_UTF8, -1, rs->stringAt(i)) ||
	     !tail.append(h) )
	  return FALSE;
      }
      return tail.close();
    }
    case XT_VECTOR:
    { const Rexp *e;
      PlTerm_tail tail(t);
      PlTerm_var h;

      for(int i=0; e=(const Rexp*)((Rvector*)exp)->elementAt(i); i++)
      { h.put_variable();
	if ( !unify_exp(h, e) ||
	     !tail.append(h) )
	  return FALSE;
      }
      return tail.close();
    }
    case XT_STR:
    { Rstring *rs = (Rstring*)exp;
      return t.unify_chars(PL_STRING|REP_UTF8, -1, rs->string());
    }
    case XT_SYMNAME:
    { Rstring *rs = (Rstring*)exp;
      return t.unify_chars(PL_ATOM|REP_UTF8, -1, rs->string());
    }
    case XT_CLOS:
    { return t.unify_atom(ATOM_clos);
    }
    case XT_UNKNOWN:
    { return t.unify_atom(ATOM_unknown);
    }
    default:
      Sdprintf("Rexp of type %d\n", exp->type);
      return FALSE;
  }
}


		 /*******************************
		 *	    PREDICATES		*
		 *******************************/

static const PlAtom ATOM_alias("alias");
static const PlAtom ATOM_open("open");
static const PlAtom ATOM_once("once");
static const PlAtom ATOM_host("host");
static const PlAtom ATOM_port("port");

PREDICATE(r_open, 2)
{ Rref *ref;
  PlAtom alias(PlAtom::null);
  int once = FALSE;
  std::string host("127.0.0.1");
  int port = default_Rsrv_port;

  PlTerm_tail tail(A2);
  PlTerm_var opt;
  while(tail.next(opt))
  { PlAtom name(PlAtom::null);
    size_t arity;

    if ( opt.get_name_arity(&name, &arity) && arity == 1 )
    { if ( ATOM_host == name )
      { host = opt[1].as_atom().as_string();
      } else if	( ATOM_port == name )
      { port = opt[1].as_int();
      } else if ( ATOM_alias == name )
      { opt[1].get_atom_ex(&alias);
	once = TRUE;
      } else if ( ATOM_open == name )
     { PlAtom open(PlAtom::null);

	opt[1].get_atom_ex(&open);
	if ( ATOM_once == open )
	  once = TRUE;
	else
	  throw PlDomainError("open_option", opt[1]);
      }
    }
  }
  PlCheckFail(tail.close());

  if ( alias.not_null() && once )
  { PlAtom existing = get_alias(alias);

    if ( existing.not_null() )
    { Rref *eref;

      if ( (eref=symbol_Rref(existing)) &&
	   (eref->flags&R_OPEN_ONCE) )
	return A1.unify_atom(existing);
    }
  }

  ref = (Rref *)Plx_malloc(sizeof *ref);
  memset(ref, 0, sizeof *ref);

  ref->rc = new Rconnection(host.c_str(), port);
  sisocks_ok(ref->rc->connect());
  ref->name = alias;
  if ( once )
    ref->flags |= R_OPEN_ONCE;

  return unify_R_ref(A1, ref);
}


PREDICATE(r_close, 1)
{ Rref *ref;

  get_Rref(A1, &ref);
  Rconnection *rc = ref->rc;

  ref->rc = nullptr;
  ref->flags |= R_DESTROYED;
  if ( ref->name.not_null() )
  { unalias(ref->name);
    ref->name.reset();
  }

  delete rc;
  return TRUE;
}


PREDICATE(r_assign_, 3)
{ Rref *ref;
  size_t len;
  std::string vname = A2.get_nchars(CVT_ATOM|CVT_STRING|CVT_NUMBER|CVT_EXCEPTION|REP_UTF8);

  get_Rref(A1, &ref);
  PlRExp *exp = term_to_rexp(A3);
  try
  { ref->rc->assign(vname.c_str(), exp->exp);
  } catch(...)
  { delete(exp);
    throw;
  }
  delete(exp);
  return TRUE;
}


PREDICATE(r_eval, 2)
{ Rref *ref;
  std::string command = A2.get_nchars(CVT_ATOM|CVT_STRING|CVT_EXCEPTION|REP_UTF8);
  int rc;
  int status = 0;

  get_Rref(A1, &ref);
  ref->rc->eval(command.c_str(), &status, 1);
  if ( status == 0 )
    return TRUE;
  throw RError(status);
}


PREDICATE(r_eval, 3)
{ Rref *ref;
  std::string command = A2.get_nchars(CVT_ATOM|CVT_STRING|CVT_EXCEPTION|REP_UTF8);
  int rc;
  int status = 0;

  get_Rref(A1, &ref);
  Rexp *result = ref->rc->eval(command.c_str(), &status);
  if ( result )
  { try
    { rc = unify_exp(A3, result);
    } catch(...)
    { delete(result);
      throw;
    }
    delete result;
    return rc;
  }

  throw RError(status);
}


PREDICATE(r_read_file, 3)
{ Rref *ref;
  std::string filename = A2.get_nchars(CVT_ATOM|CVT_STRING|CVT_EXCEPTION|REP_UTF8);
  char buf[4096];
  std::string data;
  int rc;

  get_Rref(A1, &ref);
  rok(ref->rc->openFile(filename.c_str()));
  data.reserve(sizeof buf);
  while((rc=ref->rc->readFile(buf, sizeof buf)) > 0)
  { data.append(buf, (size_t)rc);
  }
  rok(ref->rc->closeFile());

  return A3.unify_chars(PL_STRING, data.size(), data.data());
}


PREDICATE(r_remove_file, 2)
{ Rref *ref;
  std::string filename = A2.get_nchars(CVT_ATOM|CVT_STRING|CVT_EXCEPTION|REP_UTF8);
  int rc;

  get_Rref(A1, &ref);
  rok(ref->rc->removeFile(filename.c_str()));

  return TRUE;
}


PREDICATE(r_login, 3)
{ Rref *ref;
  std::string user = A2.get_nchars(CVT_ATOM|CVT_STRING|CVT_EXCEPTION|REP_UTF8);
  std::string password = A3.get_nchars(CVT_ATOM|CVT_STRING|CVT_EXCEPTION|REP_UTF8);

  get_Rref(A1, &ref);
  rok(ref->rc->login(user.c_str(), password.c_str()));

  return TRUE;
}


static int
hex_byte(int val)
{ if ( val < 10 )
    return '0'+val;
  else
    return 'a'+val-10;
}

static void
tohex(char *to, const char *from, int len)
{ for(int i=0; i<len; i++)
  { *to++ = hex_byte((from[i]>>4)&0xf);
    *to++ = hex_byte(from[i]&0xf);
  }
  *to = '\0';
}


static int
unhexb(int c)
{ if ( c >= '0' && c <= '9' )
    return c-'0';
  if ( c >= 'a' && c <= 'f' )
    return c-'a'+10;
  if ( c >= 'A' && c <= 'F' )
    return c-'A'+10;
  return -1;
}

static int
unhex(char *to, const char *from, int size)
{ for(; *from; from += 2)
  { if ( --size < 0 || !from[1] )
      return -1;

    int d1 = unhexb(from[0]);
    int d2 = unhexb(from[1]);
    if ( d1 < 0 || d2 < 0 )
      return -1;

    *to++ = (d1<<4) + d2;
  }

  return 0;
}


PREDICATE(r_detach_, 2)
{ Rref *ref;
  int status = 0;

  get_Rref(A1, &ref);
  std::unique_ptr<Rsession> session(ref->rc->detach(&status));

  if ( session )
  { char hkey[65];
    int rc;

    tohex(hkey, session->key(), 32);
    return A2.unify_term(PlCompound("r_session", PlTermv(PlTerm_string(session->host()),
							 PlTerm_integer(session->port()),
							 PlTerm_string(hkey))));
  }

  throw RError(status);
}


static const PlFunctor FUNCTOR_r_session3("r_session", 3);

PREDICATE(r_resume, 3)
{ if ( A2.is_functor(FUNCTOR_r_session3) )
  { char key[32];
    std::string host = A2[1].get_nchars(CVT_ATOM|CVT_STRING|CVT_EXCEPTION|REP_UTF8);
    int   port = A2[2].as_int();
    std::string hkey = A2[3].get_nchars(CVT_ATOM|CVT_STRING|CVT_EXCEPTION|REP_UTF8);
    Rref *ref;
    PlAtom alias(PlAtom::null);

    if ( !A3.is_variable() )
      A3.get_atom_ex(&alias);

    if ( unhex(key, hkey.c_str(), sizeof key) < 0 )
      throw PlDomainError("r_session_key", A2[3]);
    Rsession session(host.c_str(), port, key);

    ref = (Rref *)Plx_malloc(sizeof *ref);
    memset(ref, 0, sizeof *ref);

    ref->rc = new Rconnection(&session);
    sisocks_ok(ref->rc->connect());
    ref->name = alias;

    return unify_R_ref(A1, ref);
  }

  throw PlTypeError("r_session", A2);
}


#ifdef CMD_ctrl
PREDICATE(r_server_eval, 2)
{ Rref *ref;
  std::string command = A2.get_nchars(CVT_ATOM|CVT_STRING|CVT_EXCEPTION|REP_UTF8);

  get_Rref(A1, &ref);
  rok(ref->rc->serverEval(command.c_str()));

  return TRUE;
}

PREDICATE(r_server_source, 2)
{ Rref *ref;
  std::string filename = A2.get_nchars(CVT_ATOM|CVT_STRING|CVT_EXCEPTION|REP_UTF8);

  get_Rref(A1, &ref);
  rok(ref->rc->serverEval(filename.c_str()));

  return TRUE;
}

PREDICATE(r_server_shutdown, 1)
{ Rref *ref;

  get_Rref(A1, &ref);
  rok(ref->rc->serverShutdown());

  return TRUE;
}
#endif
