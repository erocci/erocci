/*
  Erlang port driver for htpasswd-like functions

  Copyright 2014 Jean Parpaillon

  Author: Jean Parpaillon <jean.parpaillon@free.fr>
*/
#include <apr_md5.h>

#include "htpasswd.h"

ERL_NIF_TERM
nif_validate_password(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  htpasswd_st* st = (htpasswd_st*)enif_priv_data(env);
  ERL_NIF_TERM ret;
  apr_status_t rv;
  char passwd[MAXBUFLEN] = {0};
  char hash[MAXBUFLEN] = {0};

  if (argc != 2) {
    ret = enif_make_badarg(env);
    goto out;
  }

  if (enif_get_string(env, argv[0], passwd, MAXBUFLEN, ERL_NIF_LATIN1) < 1) {
    ret = enif_make_badarg(env);
    goto out;
  }

  if (enif_get_string(env, argv[1], hash, MAXBUFLEN, ERL_NIF_LATIN1) < 1) {
    ret = enif_make_badarg(env);
    goto out;
  }
  
  rv = apr_password_validate(passwd, hash);
  if (rv == APR_SUCCESS)
    ret = st->atom_true;
  else
    ret = st->atom_false;
  
 out:
  return ret;
}

/*
 * NIF callbacks
 */
static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    htpasswd_st* st = enif_alloc(sizeof(htpasswd_st));
    if(st == NULL) {
        return 1;
    }

    st->atom_true = make_atom(env, "true");
    st->atom_false = make_atom(env, "false");

    *priv = (void*) st;

    return 0;
}

static int
reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    return load(env, priv, info);
}

static void
unload(ErlNifEnv* env, void* priv)
{
    enif_free(priv);
    return;
}

static ErlNifFunc funcs[] =
{
  {"validate_password", 2, nif_validate_password}
};


ERL_NIF_TERM
make_atom(ErlNifEnv* env, const char* name)
{
    ERL_NIF_TERM ret;
    if(enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1)) {
      return ret;
    }
    return enif_make_atom(env, name);
}

ERL_NIF_INIT(occi_authnz_mod_htpasswd, funcs, &load, &reload, &upgrade, &unload);
