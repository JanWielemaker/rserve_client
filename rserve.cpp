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
#include <assert.h>
#include <mutex>
#include "cxx/sisocks.h"
#include "cxx/Rconnection.h"
#include <SWI-Stream.h>
#include <SWI-cpp.h>

		 /*******************************
		 *	       SYMBOL		*
		 *******************************/

#define R_DESTROYED	0x0001		/* Was destroyed by user  */
#define R_OPEN_ONCE	0x0002		/* Reuse alias */

typedef struct Rref
{ Rconnection   *rc;			/* Connection handle */
  atom_t         symbol;		/* associated symbol */
  atom_t	 name;			/* alias name */
  int	         flags;			/* flags */
} Rref;


		 /*******************************
		 *	      ALIAS		*
		 *******************************/

#define NULL_ATOM (atom_t)0

typedef struct alias_cell
{ atom_t	name;
  atom_t	symbol;
  struct alias_cell *next;
} alias_cell;

#define ALIAS_HASH_SIZE 64

std::mutex alias_lock;
static unsigned int alias_size = ALIAS_HASH_SIZE;
static alias_cell *alias_entries[ALIAS_HASH_SIZE];

static unsigned int
atom_hash(atom_t a)
{ return (unsigned int)(a>>7) % alias_size;
}

static atom_t
get_alias(atom_t name)
{ for(alias_cell *c = alias_entries[atom_hash(name)];
      c;
      c = c->next)
  { if ( c->name == name )
      return c->symbol;
  }

  return NULL_ATOM;
}

static void
alias(atom_t name, atom_t symbol)
{ unsigned int key = atom_hash(name);

  alias_lock.lock();
  if ( !get_alias(name) )
  { alias_cell *c = (alias_cell *)malloc(sizeof(*c));

    c->name   = name;
    c->symbol = symbol;
    c->next   = alias_entries[key];
    alias_entries[key] = c;
    PL_register_atom(c->name);
    PL_register_atom(c->symbol);
    alias_lock.unlock();
  } else
  { alias_lock.unlock();
    throw PlPermissionError("alias", "rocksdb", PlTerm(name));
  }
}

static void
unalias(atom_t name)
{ unsigned int key = atom_hash(name);
  alias_cell *c, *prev=NULL;

  alias_lock.lock();
  for(c = alias_entries[key]; c; prev=c, c = c->next)
  { if ( c->name == name )
    { if ( prev )
	prev->next = c->next;
      else
	alias_entries[key] = c->next;
      PL_unregister_atom(c->name);
      PL_unregister_atom(c->symbol);
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
{ Rref **refp = (Rref **)PL_blob_data(symbol, NULL, NULL);
  Rref *ref = *refp;
  (void)flags;

  Sfprintf(s, "<Rserve>(%p)", ref);
  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
GC an rocks from the atom garbage collector.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
release_R_ref(atom_t symbol)
{ Rref **refp = (Rref **)PL_blob_data(symbol, NULL, NULL);
  Rref *ref   = *refp;
  Rconnection *rc;

  assert(ref->name == NULL_ATOM);

  if ( (rc=ref->rc) )
  { ref->rc = NULL;
    delete rc;
  }
  PL_free(ref);

  return TRUE;
}


static int
save_R_ref(atom_t symbol, IOSTREAM *fd)
{ Rref **refp = (Rref **)PL_blob_data(symbol, NULL, NULL);
  Rref *ref   = *refp;
  (void)fd;

  return PL_warning("Cannot save reference to <Rserve>(%p)", ref);
}


static atom_t
load_R_ref(IOSTREAM *fd)
{ (void)fd;

  return PL_new_atom("<saved-Rserve-ref>");
}


static PL_blob_t R_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE,
  (char*)"rocksdb",
  release_R_ref,
  NULL,
  write_R_ref,
  NULL,
  save_R_ref,
  load_R_ref
};


static int
unify_R_ref(term_t t, Rref *ref)
{ if ( ref->name )
  { if ( !ref->symbol )
    { PlTerm tmp;

      if ( PL_unify_blob(tmp, &ref, sizeof(ref), &R_blob) &&
	   PL_get_atom(tmp, &ref->symbol) )
      { alias(ref->name, ref->symbol);
      } else
      { assert(0);
      }
    }
    return PL_unify_atom(t, ref->name);
  } else if ( ref->symbol )
  { return PL_unify_atom(t, ref->symbol);
  } else
  { return ( PL_unify_blob(t, &ref, sizeof(ref), &R_blob) &&
	     PL_get_atom(t, &ref->symbol)
	   );
  }
}


static Rref*
symbol_Rref(atom_t symbol)
{ void *data;
  size_t len;
  PL_blob_t *type;

  if ( (data=PL_blob_data(symbol, &len, &type)) && type == &R_blob )
  { Rref **erd = (Rref **)data;
    return *erd;
  }

  return (Rref*)NULL;
}


static int
get_Rref(term_t t, Rref **erp, int warn=TRUE)
{ atom_t a;

  if ( PL_get_atom(t, &a) )
  { for(int i=0; i<2; i++)
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

static const char *
sisocks_msg(int rc)
{ static char msg[128];				/* TBD: thread safety */

  sockerrorchecks(msg, sizeof(msg), -1);

  return msg;
}

class SISocksError : public PlException
{
public:
  SISocksError(int rc) :
    PlException(PlCompound("error",
			   PlTermv(PlCompound("sisocks_error",
					      PlTermv((long)rc,
						      sisocks_msg(rc))),
				   PlTerm())))
  {
  }
};


static void
sisocks_ok(int status)
{ if ( status )
    throw SISocksError(status);
}


typedef enum dtype
{ D_UNKNOWN,
  D_INTEGER,
  D_DOUBLE,
} dtype;

static void
set_type(const PlTerm &t, dtype *type, dtype to)
{ if ( to >= *type )
  { *type = to;
    return;
  }

  throw PlTypeError("Rdata", t);
}

static int
classify_list(const PlTerm &t, size_t *len)
{ dtype type = D_UNKNOWN;

  switch(PL_skip_list(t, 0, len))
  { case PL_LIST:
      break;
    case PL_PARTIAL_LIST:
      throw PlInstantiationError();
    case PL_CYCLIC_TERM:
    case PL_NOT_A_LIST:
    default:
      throw PlTypeError("list", t);
  }

  PlTail tail(t);
  PlTerm e;
  while(tail.next(e))
  { switch(PL_term_type(e))
    { case PL_VARIABLE:
	throw PlInstantiationError(e);
      case PL_INTEGER:
	set_type(e, &type, D_INTEGER);
        break;
      case PL_FLOAT:
	set_type(e, &type, D_DOUBLE);
        break;
      default:
	throw PlTypeError("Rdata", e);
    }
  }

  return type;
}

static void
get_array(const PlTerm &t, int *array)
{ int i = 0;
  PlTail tail(t);
  PlTerm e;

  while(tail.next(e))
    array[i++] = e;
}


static int
unify_exp(const PlTerm &t, const Rexp *exp)
{ switch(exp->type)
  { case XT_ARRAY_INT:
    { Rinteger *ri = (Rinteger*)exp;
      Rsize_t len = ri->length();
      PlTail tail(t);
      PlTerm h;

      for(Rsize_t i=0; i<len; i++)
      { if ( !PL_put_integer(h, ri->intAt(i)) ||
	     !tail.append(h) )
	  return FALSE;
      }
      return tail.close();
    }
    case XT_ARRAY_DOUBLE:
    { Rdouble *rd = (Rdouble*)exp;
      Rsize_t len = rd->length();
      PlTail tail(t);
      PlTerm h;

      for(Rsize_t i=0; i<len; i++)
      { if ( !PL_put_float(h, rd->doubleAt(i)) ||
	     !tail.append(h) )
	  return FALSE;
      }
      return tail.close();
    }
    case XT_VECTOR:
    { const Rexp *e;
      PlTail tail(t);
      PlTerm h;

      for(int i=0; e=(const Rexp*)((Rvector*)exp)->elementAt(i); i++)
      { PL_put_variable(h);
	if ( !unify_exp(h, e) ||
	     !tail.append(h) )
	  return FALSE;
      }
      return tail.close();
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

PREDICATE(r_open, 2)
{ Rref *ref;
  atom_t alias = NULL_ATOM;
  int once = FALSE;

  PlTail tail(A2);
  PlTerm opt;
  while(tail.next(opt))
  { atom_t name;
    size_t arity;

    if ( PL_get_name_arity(opt, &name, &arity) && arity == 1 )
    { if ( ATOM_alias == name )
      { if ( !PL_get_atom_ex(opt[1], &alias) )
	  return FALSE;
	once = TRUE;
      } else if ( ATOM_open == name )
      { atom_t open;

	if ( !PL_get_atom_ex(opt[1], &open) )
	  return FALSE;
	if ( ATOM_once == open )
	  once = TRUE;
	else
	  throw PlDomainError("open_option", opt[1]);
      }
    }
  }

  if ( alias && once )
  { atom_t existing;

    if ( (existing=get_alias(alias)) )
    { Rref *eref;

      if ( (eref=symbol_Rref(existing)) &&
	   (eref->flags&R_OPEN_ONCE) )
	return PL_unify_atom(A1, existing);
    }
  }

  ref = (Rref *)PL_malloc(sizeof(*ref));
  memset(ref, 0, sizeof(*ref));

  ref->rc = new Rconnection();
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

  ref->rc = NULL;
  ref->flags |= R_DESTROYED;
  if ( ref->name )
  { unalias(ref->name);
    ref->name = NULL_ATOM;
  }

  delete rc;
  return TRUE;
}


PREDICATE(r_set, 3)
{ Rref *ref;
  size_t len;
  const char *vname = A2;

  get_Rref(A1, &ref);
  switch(classify_list(A3, &len))
  { case D_INTEGER:
    { int *iv = new int[len];

      get_array(A3, iv);
      Rinteger *ri = new Rinteger(iv, len);
      ref->rc->assign(vname, ri);
      delete ri;
      delete iv;
      return TRUE;
    }
  }
}


PREDICATE(r_eval, 3)
{ Rref *ref;
  const char *command = A2;
  int rc;

  get_Rref(A1, &ref);
  Rexp *result = ref->rc->eval(command);
  rc = unify_exp(A3, result);
  delete result;

  return rc;
}
