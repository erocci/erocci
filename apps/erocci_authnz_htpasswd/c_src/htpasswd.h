/*
  Erlang port driver for checking password against htpasswd hashes

  Copyright 2014 Jean Parpaillon

  Author: Jean Parpaillon <jean.parpaillon@free.fr>
*/

#ifndef __HTPASSWD_DRV__
#define __HTPASSWD_DRV__

#include <apr_md5.h>

#include "erl_nif.h"

#define MAXBUFLEN 1024

typedef struct {
  ERL_NIF_TERM    atom_true;
  ERL_NIF_TERM    atom_false;
} htpasswd_st;

ERL_NIF_TERM make_atom(ErlNifEnv* env, const char* name);
ERL_NIF_TERM nif_validate_password(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif
