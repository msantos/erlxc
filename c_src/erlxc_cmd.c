/* Copyright (c) 2013, Michael Santos <michael.santos@gmail.com>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
#include "erlxc.h"

static int erlxc_list_to_argv(char ***, ETERM *);
static void erlxc_free_argv(char ***);

/* commands */
enum {
    ERLXC_LIST_ACTIVE_CONTAINERS = 1,
    ERLXC_LIST_ALL_CONTAINERS,
    ERLXC_LIST_DEFINED_CONTAINERS,

    ERLXC_LXC_CONTAINER_NEW,
    ERLXC_LXC_CONTAINER_START,
    ERLXC_LXC_CONTAINER_STOP,
    ERLXC_LXC_CONTAINER_SET_CONFIG_ITEM,
    ERLXC_LXC_CONTAINER_LOAD_CONFIG,
};

/* Options */
enum {
    ERLXC_TERMINATE_ON_EXIT = 1 << 0,   /* Destroy the container when the port stops */
};

    ETERM *
erlxc_cmd(erlxc_state_t *ep, u_int32_t cmd, ETERM *arg)
{
    VERBOSE(2, "cmd=%d", cmd);
    if (ep->verbose >= 2)
        erl_print_term(stderr, arg);

    switch (cmd) {
        case ERLXC_LIST_ACTIVE_CONTAINERS:
            return erlxc_list_containers(ep, arg, list_active_containers);

        case ERLXC_LIST_ALL_CONTAINERS:
            return erlxc_list_containers(ep, arg, list_all_containers);

        case ERLXC_LIST_DEFINED_CONTAINERS:
            return erlxc_list_containers(ep, arg, list_defined_containers);

        case ERLXC_LXC_CONTAINER_NEW:
            return erlxc_lxc_container_new(ep, arg);

        case ERLXC_LXC_CONTAINER_START:
            return erlxc_lxc_container_start(ep, arg);

        case ERLXC_LXC_CONTAINER_STOP:
            return erlxc_lxc_container_stop(ep, arg);

        case ERLXC_LXC_CONTAINER_LOAD_CONFIG:
            return erlxc_lxc_container_load_config(ep, arg);

        default:
            break;
    }

    return erlxc_error("einval");
}

    ETERM *
erlxc_lxc_container_new(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    struct lxc_container *c = NULL;
    char *name = NULL;
    char *path = NULL;

    if (ep->cur >= ep->max)
        return erlxc_error("emfile");

    if (!ERL_IS_LIST(arg) || erl_length(arg) != 2)
        goto BADARG;

    arg = erlxc_list_head(&hd, arg);

    if (!hd)
        goto BADARG;

    if (erl_iolist_length(hd) == 0)
        goto BADARG;

    name = erl_iolist_to_string(hd);
    if (!name)
        goto BADARG;

    arg = erlxc_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    if (erl_iolist_length(hd) > 0) {
        path = erl_iolist_to_string(hd);
        if (!path)
            goto BADARG;
    }

    c = lxc_container_new(name, path);

    if (!c)
        goto BADARG;

    erl_free(name);
    erl_free(path);

    ep->c[ep->cur] = c;

    return erlxc_ok(erl_mk_int(ep->cur++));

BADARG:
    erl_free(name);
    erl_free(path);
    return erl_mk_atom("badarg");
}

    ETERM *
erlxc_lxc_container_start(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    struct lxc_container *c = NULL;
    int useinit = 0;
    char **argv = NULL;
    bool res;
    int errnum = 0;
    pid_t pid = -1;

    char *config_file_name = NULL;

    // bool (*start)(struct lxc_container *c, int useinit, char * const argv[]);

    erl_print_term(stderr, arg);

    if (!ERL_IS_LIST(arg) || erl_length(arg) != 3)
        goto BADARG;

    /* cid */
    arg = erlxc_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    c = erlxc_cid(ep, ERL_INT_VALUE(hd));
    if (!c)
        return erlxc_error("einval");

    /* useinit */
    arg = erlxc_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    useinit = ERL_INT_VALUE(hd);

    /* argv */
    if (erlxc_list_to_argv(&argv, arg) < 0)
        goto BADARG;

    config_file_name = c->config_file_name(c);
    VERBOSE(2, "see this: name=%s, useinit=%d, config_file_name=%s", c->name, useinit, config_file_name);

    if (!lxc_container_get(c))
        goto BADARG;

    pid = fork();

    switch (pid) {
        case -1:
            return erlxc_errno(errnum);
        case 0:
            res = c->start(c, useinit, argv);
            (void)res;
            erl_err_quit("start failed");
        default:
            erlxc_free_argv(&argv);
            return erlxc_tuple2(erl_mk_atom("ok"), erl_mk_int(pid));
    }
    
BADARG:
    erlxc_free_argv(&argv);
    return erl_mk_atom("badarg");
}

    ETERM *
erlxc_lxc_container_stop(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    struct lxc_container *c = NULL;
    bool res;
    int errnum = 0;

    if (!ERL_IS_LIST(arg) || erl_length(arg) != 1)
        goto BADARG;

    arg = erlxc_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    c = erlxc_cid(ep, ERL_INT_VALUE(hd));
    if (!c)
        return erlxc_error("einval");

    errno = 0;
    res = c->stop(c);
    errnum = errno;

    lxc_container_put(c);
    return (res ? erl_mk_atom("ok") : erlxc_errno(errnum));

BADARG:
    lxc_container_put(c);
    return erl_mk_atom("badarg");
}

    ETERM *
erlxc_lxc_container_load_config(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    struct lxc_container *c = NULL;
    char *path = NULL;

    if (!ERL_IS_LIST(arg) || erl_length(arg) != 2)
        goto BADARG;

    arg = erlxc_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    c = erlxc_cid(ep, ERL_INT_VALUE(hd));
    if (!c)
        return erlxc_error("einval");

    arg = erlxc_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    if (erl_iolist_length(hd) > 0) {
        path = erl_iolist_to_string(hd);
        if (!path)
            goto BADARG;
    }

    errno = 0;
    if (c->load_config(c, path))
        return erl_mk_atom("ok");
    else
        return erlxc_errno(errno);

BADARG:
    return erl_mk_atom("badarg");
}

    ETERM *
erlxc_list_containers(erlxc_state_t *ep, ETERM *arg,
        int (*fun)(const char *path, char ***names, struct lxc_container ***cr))
{
    int n = -1;
    int i = 0;
    ETERM *hd = NULL;
    char *path = NULL;
    char **names = NULL;
    ETERM **reply = NULL;
    int errnum = 0;

    if (!ERL_IS_LIST(arg) || erl_length(arg) != 1)
        goto BADARG;

    arg = erlxc_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    if (erl_iolist_length(hd) > 0) {
        path = erl_iolist_to_string(hd);
        if (!path)
            goto BADARG;
    }
    
    errno = 0;
    n = fun(path, &names, NULL);
    errnum = errno;
    
    //erl_free_term(arg1);
    erl_free(path);
    
    if (n < 0)
        return erlxc_errno(errnum);
    
    reply = erl_malloc(n * sizeof(ETERM **));
    for (i = 0; i < n; i++) {
        reply[i] = erl_mk_binary(names[i], strnlen(names[i], MAXHOSTNAMELEN));
        free(names[i]);
    }
    
    if (n > 0)
        free(names);
    
    return erlxc_ok(erl_mk_list(reply, n));

BADARG:
    return erl_mk_atom("badarg");
}

    struct lxc_container *
erlxc_cid(erlxc_state_t *ep, int cid)
{
    if (cid < 0 || cid > ep->max)
        return NULL;

    return ep->c[cid];
}

    static int
erlxc_list_to_argv(char ***argv, ETERM *arg)
{
    int len = 0; /* XXX overflow */
    int i = 0;

    len = erl_length(arg);
    /* NULL terminate */
    *argv = erl_malloc((len + 1) * sizeof(char **));
    (void)memset(*argv, 0, len+1);

    if (!*argv)
        return -1;

    for (i = 0; i < len; i++) {
        *argv[i] = erl_iolist_to_string(arg);
        /* XXX free/crash here */
        if (!*argv[i])
            return -1;
    }

    return 0;
}

    static void
erlxc_free_argv(char ***argv)
{
    int i = 0;

    if (argv == NULL)
        return;

    for (i = 0; *argv[i] != NULL; i++)
        erl_free(*argv[i]);
}
