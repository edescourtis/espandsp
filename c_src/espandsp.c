#include <string.h>
#include <limits.h>
#include <stdint.h>
#include <assert.h>
#include <arpa/inet.h>
#include <spandsp.h>
#include <spandsp/g711.h>
#include <spandsp/private/g711.h>
#include <spandsp/g722.h>
#include <spandsp/private/g722.h>
#include "erl_nif.h"

static ErlNifResourceType *spandsp_g711_type = NULL;
static ErlNifResourceType *spandsp_g722_encode_type = NULL;
static ErlNifResourceType *spandsp_g722_decode_type = NULL;


static ERL_NIF_TERM mk_atom(ErlNifEnv* env, const char* atom)
{
    ERL_NIF_TERM ret;

    if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1)){
        return enif_make_atom(env, atom);
    }

    return ret;
}

static ERL_NIF_TERM mk_error(ErlNifEnv* env, const char* mesg)
{
    return enif_make_tuple2(env, mk_atom(env, "error"), mk_atom(env, mesg));
}

static ERL_NIF_TERM output_encoding_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 0) return enif_make_badarg(env);
    return (htons(1) == 1) ? mk_atom(env, "pcm_s16be"): mk_atom(env, "pcm_s16le");
}

static ERL_NIF_TERM g711_open_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ERL_NIF_TERM resource;
    g711_state_t *state;
    int mode;
    char modebuf[5];

    if(argc != 1) return enif_make_badarg(env);

    if(!enif_is_atom(env, argv[0]))
        return enif_make_badarg(env);
    if(!enif_get_atom(env, argv[0], modebuf, sizeof(modebuf), ERL_NIF_LATIN1))
        return enif_make_badarg(env);

    if     (strcmp(modebuf, "ulaw") == 0) mode = G711_ULAW;
    else if(strcmp(modebuf, "alaw") == 0) mode = G711_ALAW;
         else return enif_make_badarg(env);

    state = (g711_state_t *)enif_alloc_resource(spandsp_g711_type, sizeof(g711_state_t));
    if(g711_init(state, mode) == NULL){
        g711_release(state);
        enif_release_resource(state);
        return mk_error(env, "g711_init");
    }
    resource = enif_make_resource(env, state);
    enif_release_resource(state);

    return resource;
}

static ERL_NIF_TERM g711_encode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    size_t len;
    g711_state_t *state = NULL;
    ErlNifBinary bin;
    ErlNifBinary out_bin;

    if(argc != 2) return enif_make_badarg(env);

    if(!enif_get_resource(env, argv[0], spandsp_g711_type, (void **)&state))
        return enif_make_badarg(env);

    assert(state != NULL);

    if(!enif_inspect_binary(env, argv[1], &bin))
        return enif_make_badarg(env);

    len = bin.size / sizeof(int16_t);
    if(len > (size_t)INT_MAX) return enif_make_badarg(env);

    if(!enif_alloc_binary(len, &out_bin)) return enif_make_badarg(env);

    (void)g711_encode(state, (uint8_t *)out_bin.data, (int16_t *)bin.data, (int)len);

    return enif_make_binary(env, &out_bin);
}

static ERL_NIF_TERM g711_decode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    size_t len;
    g711_state_t *state = NULL;
    ErlNifBinary bin;
    ErlNifBinary out_bin;

    if(argc != 2) return enif_make_badarg(env);

    if(!enif_get_resource(env, argv[0], spandsp_g711_type, (void **)&state))
        return enif_make_badarg(env);

    assert(state != NULL);

    if(!enif_inspect_binary(env, argv[1], &bin))
        return enif_make_badarg(env);

    len = bin.size * sizeof(int16_t);
    if(len > (size_t)INT_MAX) return enif_make_badarg(env);

    if(!enif_alloc_binary(len, &out_bin)) return enif_make_badarg(env);

    (void)g711_decode(state, (int16_t *)out_bin.data, (uint8_t *)bin.data, (int)bin.size);

    return enif_make_binary(env, &out_bin);
}

static ERL_NIF_TERM g711_transcode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    g711_state_t *state = NULL;
    ErlNifBinary bin;
    ErlNifBinary out_bin;

    if(argc != 2) return enif_make_badarg(env);

    if(!enif_get_resource(env, argv[0], spandsp_g711_type, (void **)&state))
        return enif_make_badarg(env);

    assert(state != NULL);

    if(!enif_inspect_binary(env, argv[1], &bin))
        return enif_make_badarg(env);

    if(bin.size > (size_t)INT_MAX) return enif_make_badarg(env);

    if(!enif_alloc_binary(bin.size, &out_bin)) return enif_make_badarg(env);

    (void)g711_transcode(state, (uint8_t *)out_bin.data, (uint8_t *)bin.data, (int)bin.size);

    return enif_make_binary(env, &out_bin);
}

static ERL_NIF_TERM g722_encode_init_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    g722_encode_state_t *state = NULL;
    g722_encode_state_t *ctx;
    ERL_NIF_TERM head;
    ERL_NIF_TERM tail;
    ERL_NIF_TERM list;
    ERL_NIF_TERM resource;
    int rate;
    int options = 0;
    unsigned len;
    unsigned i;
    char buf[64];

    if(argc != 2) return enif_make_badarg(env);

    if(!enif_get_int(env, argv[0], &rate)){
        return enif_make_badarg(env);
    }

    list = argv[1];

    if(!enif_get_list_length(env, list, &len)){
        return enif_make_badarg(env);
    }

    for(i = 0; i < len; i++){
        if(!enif_get_list_cell(env, list, &head, &tail)){
            return enif_make_badarg(env);
        }
        list = tail;
        if(!enif_get_atom(env, head, buf, (unsigned)sizeof(buf), ERL_NIF_LATIN1)){
            return enif_make_badarg(env);
        }

        if(strcmp(buf, "sample_rate_8000") == 0){
            options |= G722_SAMPLE_RATE_8000;
        }else if(strcmp(buf, "packed") == 0){
            options |= G722_PACKED;
        }else{
            return enif_make_badarg(env);
        }
    }

    switch(rate){
    case 64000:
    case 56000:
    case 48000:
        break;
    default:
        return mk_error(env, "unsupported_bit_rate");
    }

    state = (g722_encode_state_t *)enif_alloc_resource(
        spandsp_g722_encode_type,
        sizeof(g722_encode_state_t)
    );

    ctx = g722_encode_init(state, rate, options);
    if(ctx == NULL){
        enif_release_resource(state);
        return mk_error(env, "g722_encode_init");
    }

    resource = enif_make_resource(env, state);
    enif_release_resource(state);
    return resource;
}

static ERL_NIF_TERM g722_encode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    size_t len;
    g722_encode_state_t *state = NULL;
    ErlNifBinary bin;
    ErlNifBinary out_bin;

    if(argc != 2) return enif_make_badarg(env);

    if(!enif_get_resource(env, argv[0], spandsp_g722_encode_type, (void **)&state))
        return enif_make_badarg(env);

    assert(state != NULL);

    if(!enif_inspect_binary(env, argv[1], &bin))
        return enif_make_badarg(env);

    len = bin.size / sizeof(int16_t);
    if(len > (size_t)INT_MAX) return enif_make_badarg(env);

    if(!enif_alloc_binary(len, &out_bin)) return enif_make_badarg(env);

    (void)g722_encode(state, (uint8_t *)out_bin.data, (int16_t *)bin.data, (int)len);

    return enif_make_binary(env, &out_bin);
}

static ERL_NIF_TERM g722_decode_init_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    g722_decode_state_t *state = NULL;
    g722_decode_state_t *ctx;
    ERL_NIF_TERM head;
    ERL_NIF_TERM tail;
    ERL_NIF_TERM list;
    ERL_NIF_TERM resource;
    int rate;
    int options = 0;
    unsigned len;
    unsigned i;
    char buf[64];

    if(argc != 2) return enif_make_badarg(env);

    if(!enif_get_int(env, argv[0], &rate)){
        return enif_make_badarg(env);
    }

    list = argv[1];

    if(!enif_get_list_length(env, list, &len)){
        return enif_make_badarg(env);
    }

    for(i = 0; i < len; i++){
        if(!enif_get_list_cell(env, list, &head, &tail)){
            return enif_make_badarg(env);
        }
        list = tail;
        if(!enif_get_atom(env, head, buf, (unsigned)sizeof(buf), ERL_NIF_LATIN1)){
            return enif_make_badarg(env);
        }

        if(strcmp(buf, "sample_rate_8000") == 0){
            options |= G722_SAMPLE_RATE_8000;
        }else if(strcmp(buf, "packed") == 0){
            options |= G722_PACKED;
        }else{
            return enif_make_badarg(env);
        }
    }

    switch(rate){
    case 64000:
    case 56000:
    case 48000:
        break;
    default:
        return mk_error(env, "unsupported_bit_rate");
    }

    state = (g722_decode_state_t *)enif_alloc_resource(
        spandsp_g722_decode_type,
        sizeof(g722_decode_state_t)
    );

    ctx = g722_decode_init(state, rate, options);
    if(ctx == NULL){
        enif_release_resource(state);
        return mk_error(env, "g722_decode_init");
    }

    resource = enif_make_resource(env, state);
    enif_release_resource(state);
    return resource;
}

static ERL_NIF_TERM g722_decode_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    size_t len;
    g722_decode_state_t *state = NULL;
    ErlNifBinary bin;
    ErlNifBinary out_bin;

    if(argc != 2) return enif_make_badarg(env);

    if(!enif_get_resource(env, argv[0], spandsp_g722_decode_type, (void **)&state))
        return enif_make_badarg(env);

    assert(state != NULL);

    if(!enif_inspect_binary(env, argv[1], &bin))
        return enif_make_badarg(env);

    len = bin.size * sizeof(int16_t);
    if(len > (size_t)INT_MAX) return enif_make_badarg(env);

    if(!enif_alloc_binary(len, &out_bin)) return enif_make_badarg(env);

    (void)g722_decode(state, (int16_t *)out_bin.data, (uint8_t *)bin.data, (int)bin.size);
    return enif_make_binary(env, &out_bin);
}


static void garbage_collect_spandsp_g711(ErlNifEnv *env, void *p)
{
    g711_state_t *state;

    state = (g711_state_t *)p;
    g711_release(state);
}

static void garbage_collect_spandsp_g722_encode(ErlNifEnv *env, void *p)
{
    g722_encode_state_t *state;

    state = (g722_encode_state_t *)p;
    g722_encode_release(state);
}

static void garbage_collect_spandsp_g722_decode(ErlNifEnv *env, void *p)
{
    g722_decode_state_t *state;

    state = (g722_decode_state_t *)p;
    g722_decode_release(state);
}

static int handle_load(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info)
{
    ErlNifResourceType *rt;
    ErlNifResourceFlags rf;

    rt = enif_open_resource_type(
        env,
        NULL,
        "spandsp_g711_type",
        garbage_collect_spandsp_g711,
        ERL_NIF_RT_CREATE,
        &rf
    );

    if(rt == NULL) return -1;

    spandsp_g711_type = rt;

    rt = enif_open_resource_type(
        env,
        NULL,
        "spandsp_g722_encode_type",
        garbage_collect_spandsp_g722_encode,
        ERL_NIF_RT_CREATE,
        &rf
    );

    if(rt == NULL) return -1;

    spandsp_g722_encode_type = rt;

    rt = enif_open_resource_type(
        env,
        NULL,
        "spandsp_g722_decode_type",
        garbage_collect_spandsp_g722_decode,
        ERL_NIF_RT_CREATE,
        &rf
    );

    if(rt == NULL) return -1;

    spandsp_g722_decode_type = rt;

    return 0;
}

static int handle_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

static ErlNifFunc nif_funcs[] = {
            {"output_encoding_nif",     0, output_encoding_nif,     0},
            {"g711_open_nif",           1, g711_open_nif,           0},
            {"g711_encode_nif",         2, g711_encode_nif,         0},
            {"g711_decode_nif",         2, g711_decode_nif,         0},
            {"g711_transcode_nif",      2, g711_transcode_nif,      0},
            {"g722_encode_init_nif",    2, g722_encode_init_nif,    0},
            {"g722_encode_nif",         2, g722_encode_nif,         0},
            {"g722_decode_init_nif",    2, g722_decode_init_nif,    0},
            {"g722_decode_nif",         2, g722_decode_nif,         0}
};

ERL_NIF_INIT(espandsp, nif_funcs, handle_load, NULL, handle_upgrade, NULL);

