#include <erl_nif.h>

void goto_next_slash(const unsigned char**);

int match_topic(const unsigned char*, const unsigned char*);

void goto_next_slash(const unsigned char** t) {
    while(**t && **t != '/') {
        (*t)++;
    }
}

int match_topic(const unsigned char* topic, const unsigned char* filter) {
    const unsigned char *t = topic;
    const unsigned char *f = filter;
    // goto the first postion that differs
    while(*t == *f) {
        if (*t == '\0')
            return 1;
        t++; f++;
    }
    unsigned char tc = *t;
    unsigned char fc = *f;
    if (tc == '\0' && fc == '\0') { // finished topic and filter
        return 1;
    } else {
        if (tc == '\0') { // finished scaning the topic
            if (fc == '#'
                || (fc =='/' && *(f+1) == '#')
                || (fc == '+' && *(f+1) == '\0')) {
                return 1;
            } else {
                return 0;
            }
        } else if (fc == '\0') { // finished scaning the filter
            unsigned char fc_ = *(f - 1); // *f_ is the last char in the filter
            if (fc_ == '#') {
                return 1;
            } else if (fc_ == '+') {
                goto_next_slash(&t);
                return *t == '\0';
            } else {
                return 0;
            }
        } else { // topic and filter both have chars
            if (fc == '#') {
                return 1;
            } else if (fc == '+') {
                goto_next_slash(&t);
                return match_topic(t, f+1); // skip the '+'
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
    t.data[t.size] = '\0';
    f.data[f.size] = '\0';
    if (match_topic((const unsigned char *)t.data, (const unsigned char *)f.data)) {
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
