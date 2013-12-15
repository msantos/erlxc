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

static ETERM *erlxc_list_active_containers(erlxc_state_t *, ETERM *);
static ETERM *erlxc_list_all_containers(erlxc_state_t *, ETERM *);
static ETERM *erlxc_list_defined_containers(erlxc_state_t *, ETERM *);
static ETERM *erlxc_lxc_container_new(erlxc_state_t *, ETERM *);
static ETERM *erlxc_lxc_container_start(erlxc_state_t *, ETERM *);
static ETERM *erlxc_lxc_container_stop(erlxc_state_t *, ETERM *);
static ETERM *erlxc_lxc_container_load_config(erlxc_state_t *, ETERM *);

static struct lxc_container *erlxc_cid(erlxc_state_t *, int);

static int erlxc_list_to_argv(char ***, ETERM *);
static void erlxc_free_argv(char ***);
static ETERM *erlxc_list_containers(erlxc_state_t *, ETERM *,
        int (*)(const char *, char ***, struct lxc_container ***));

/* commands */
typedef struct {
    ETERM *(*fp)(erlxc_state_t *, ETERM *);
    u_int8_t narg;
} erlxc_cmd_t;

erlxc_cmd_t cmds[] = {
    {erlxc_list_active_containers, 1},
    {erlxc_list_all_containers, 1},
    {erlxc_list_defined_containers, 1},
    {erlxc_lxc_container_new, 2},
    {erlxc_lxc_container_start, 3},
    {erlxc_lxc_container_stop, 1},
    {erlxc_lxc_container_load_config, 2},
};

/* Options */
enum {
    ERLXC_TERMINATE_ON_EXIT = 1 << 0,   /* Destroy the container when the port stops */
};

    ETERM *
erlxc_cmd(erlxc_state_t *ep, u_int32_t cmd, ETERM *arg)
{
    erlxc_cmd_t *fun = NULL;

    VERBOSE(2, "cmd=%d", cmd);
    if (ep->verbose >= 2)
        erl_print_term(stderr, arg);

    if (cmd >= sizeof(cmds)/sizeof(cmds[0]))
        return erlxc_error("einval");

    fun = &cmds[cmd];

    if (!ERL_IS_LIST(arg) || erl_length(arg) != fun->narg)
        return erl_mk_atom("badarg");

    return (*fun->fp)(ep, arg);
}

    static ETERM *
erlxc_lxc_container_new(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    struct lxc_container *c = NULL;
    char *name = NULL;
    char *path = NULL;

    if (ep->cur >= ep->max)
        return erlxc_error("emfile");

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

    static ETERM *
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
//            erlxc_free_argv(&argv);
            return erlxc_tuple2(erl_mk_atom("ok"), erl_mk_int(pid));
    }

BADARG:
//    erlxc_free_argv(&argv);
    return erl_mk_atom("badarg");
}

    static ETERM *
erlxc_lxc_container_stop(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    struct lxc_container *c = NULL;
    bool res;
    int errnum = 0;

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

    static ETERM *
erlxc_lxc_container_load_config(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    struct lxc_container *c = NULL;
    char *path = NULL;

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

    static ETERM *
erlxc_list_active_containers(erlxc_state_t *ep, ETERM *arg)
{
    return erlxc_list_containers(ep, arg, list_active_containers);
}

    static ETERM *
erlxc_list_all_containers(erlxc_state_t *ep, ETERM *arg)
{
    return erlxc_list_containers(ep, arg, list_all_containers);
}

    static ETERM *
erlxc_list_defined_containers(erlxc_state_t *ep, ETERM *arg)
{
    return erlxc_list_containers(ep, arg, list_defined_containers);
}

    static ETERM *
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

    static struct lxc_container *
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
