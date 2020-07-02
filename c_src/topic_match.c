#include <erl_nif.h>

void goto_next_slash(const unsigned char**, const unsigned char*);

int match_topic(const unsigned char*, const unsigned char*,
                const unsigned char*, const unsigned char*);

void goto_next_slash(const unsigned char** t, const unsigned char* tend) {
    while(*t != tend && **t != '/') {
        (*t)++;
    }
}

int match_topic(const unsigned char* topic, const unsigned char* tend,
                const unsigned char* filter, const unsigned char* fend) {
    const unsigned char *t = topic;
    const unsigned char *f = filter;
    // goto the first postion that differs
    while(*t == *f) {
        if (t == tend)
            return 1;
        t++; f++;
    }
    unsigned char fc = *f;
    if (t == tend && f == fend) { // finished topic and filter
        return 1;
    } else {
        if (t == tend) { // finished scaning the topic
            if (fc == '#'
                || (fc =='/' && *(f+1) == '#')
                || (fc == '+' && (f+1) == fend)) {
                return 1;
            } else {
                return 0;
            }
        } else if (f == fend) { // finished scaning the filter
            unsigned char fc_ = *(f - 1); // *f_ is the last char in the filter
            if (fc_ == '#') {
                return 1;
            } else if (fc_ == '+') {
                goto_next_slash(&t, tend);
                return t == tend;
            } else {
                return 0;
            }
        } else { // topic and filter both have chars
            if (fc == '#') {
                return 1;
            } else if (fc == '+') {
                goto_next_slash(&t, tend);
                return match_topic(t, tend, f+1, fend); // skip the '+'
            } else {
                return 0;
            }
        }
    }
}

static ERL_NIF_TERM match(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary t, f;
    if (!enif_inspect_binary(env, argv[0], &t)
        || !enif_inspect_binary(env, argv[1], &f)) {
        return enif_make_badarg(env);
    }
    if (t.size == 0 && f.size == 0) {
        return enif_make_atom(env, "true");
    } else if (t.size < 1 || f.size < 1) {
        return enif_make_atom(env, "false");
    }
    const unsigned char *tdata = (const unsigned char *)t.data;
    const unsigned char *fdata = (const unsigned char *)f.data;
    const unsigned char *tend = tdata + t.size;
    const unsigned char *fend = fdata + f.size;
    if (match_topic(tdata, tend, fdata, fend)) {
        return enif_make_atom(env, "true");
    }
    return enif_make_atom(env, "false");
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    *priv_data = *old_priv_data;

    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{
}

static ErlNifFunc nif_funcs[] =
{
    {"match", 2, match}
};

ERL_NIF_INIT(topic_match,nif_funcs,load,NULL,upgrade,unload)
