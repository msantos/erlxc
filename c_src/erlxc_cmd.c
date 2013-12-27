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
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/prctl.h>

#include "erlxc.h"
#include "erlxc_cmd.h"

static char **erlxc_list_to_argv(ETERM *);
static void erlxc_free_argv(char **);
static ETERM *erlxc_list_containers(erlxc_state_t *, ETERM *,
        int (*)(const char *, char ***, struct lxc_container ***));

    ETERM *
erlxc_cmd(erlxc_state_t *ep, u_int32_t cmd, ETERM *arg)
{
    erlxc_cmd_t *fun = NULL;

    VERBOSE(2, "cmd=%d", cmd);
    if (ep->verbose >= 2)
        erl_print_term(stderr, arg);

    if (cmd >= sizeof(cmds)/sizeof(cmds[0]))
        return erlxc_errno(EINVAL);

    fun = &cmds[cmd];

    if (!ERL_IS_LIST(arg) || erl_length(arg) != fun->narg)
        return erl_mk_atom("badarg");

    return (*fun->fp)(ep, arg);
}

    static ETERM *
erlxc_lxc_container_name(erlxc_state_t *ep, ETERM *arg)
{
    struct lxc_container *c = ep->c;

    if (!c)
        return erl_mk_atom("badarg");

    return erlxc_bin(c->name);
}

    static ETERM *
erlxc_lxc_container_state(erlxc_state_t *ep, ETERM *arg)
{
    struct lxc_container *c = ep->c;
    const char *state = NULL;

    if (!c)
        return erl_mk_atom("badarg");

    state = c->state(c);

    return erlxc_bin(state);
}

    static ETERM *
erlxc_lxc_container_wait(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    struct lxc_container *c = ep->c;
    char *state = NULL;
    int timeout = 0;
    bool res;

    if (!c)
        return erl_mk_atom("badarg");

    /* state */
    arg = erlxc_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        state = erl_iolist_to_string(hd);

    if (!state)
        goto BADARG;

    /* timeout */
    arg = erlxc_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    timeout = ERL_INT_VALUE(hd);
    if (timeout < 0)
        goto BADARG;

    res = c->wait(c, state, timeout);

    erl_free(state);

    return erlxc_bool(res);

BADARG:
    erl_free(state);
    return erl_mk_atom("badarg");
}

    static ETERM *
erlxc_lxc_container_daemonize(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    struct lxc_container *c = ep->c;
    int state = 0;

    if (!c)
        return erl_mk_atom("badarg");

    /* state */
    arg = erlxc_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    state = ERL_INT_VALUE(hd);
    if (state != 0 && state != 1)
        goto BADARG;

    return erlxc_bool(c->want_daemonize(c, state));

BADARG:
    return erl_mk_atom("badarg");
}

    static ETERM *
erlxc_lxc_container_defined(erlxc_state_t *ep, ETERM *arg)
{
    struct lxc_container *c = ep->c;

    if (!c)
        return erl_mk_atom("badarg");

    return erlxc_bool(c->is_defined(c));
}

    static ETERM *
erlxc_lxc_container_running(erlxc_state_t *ep, ETERM *arg)
{
    struct lxc_container *c = ep->c;

    if (!c)
        return erl_mk_atom("badarg");

    return erlxc_bool(c->is_running(c));
}

    static ETERM *
erlxc_lxc_container_create(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    struct lxc_container *c = ep->c;
    char *t = NULL;
    char *bdevtype = NULL;
    struct bdev_specs *specs = NULL;
    int flags = 0;
    char **argv = NULL;
    bool res;

    if (!c)
        return erl_mk_atom("badarg");

    /* template */
    arg = erlxc_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        t = erl_iolist_to_string(hd);

    if (!t)
        goto BADARG;

    /* bdevtype */
    arg = erlxc_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    /* specs */
    arg = erlxc_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    /* flags */
    arg = erlxc_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    flags = ERL_INT_VALUE(hd);
    flags |= LXC_CREATE_QUIET;

    /* argv */
    arg = erlxc_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    if (!ERL_IS_EMPTY_LIST(hd)) {
        argv = erlxc_list_to_argv(hd);
        if (!argv)
            goto BADARG;
    }

    res = c->create(
            c,
            t,
            bdevtype,
            specs,
            flags,
            argv
            );

    erlxc_free_argv(argv);

    return erlxc_bool(res);

BADARG:
    erlxc_free_argv(argv);
    return erl_mk_atom("badarg");
}

    static ETERM *
erlxc_lxc_container_destroy(erlxc_state_t *ep, ETERM *arg)
{
    struct lxc_container *c = ep->c;

    if (!c)
        return erl_mk_atom("badarg");

    return erlxc_bool(c->destroy(c));
}

    static ETERM *
erlxc_lxc_container_start(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    struct lxc_container *c = ep->c;
    int useinit = 0;
    char **argv = NULL;
    bool res;

    if (!c)
        return erl_mk_atom("badarg");

    /* useinit */
    arg = erlxc_list_head(&hd, arg);
    if (!hd || !ERL_IS_INTEGER(hd))
        goto BADARG;

    useinit = ERL_INT_VALUE(hd);

    /* argv */
    arg = erlxc_list_head(&hd, arg);
    if (!hd || !ERL_IS_LIST(hd))
        goto BADARG;

    if (!ERL_IS_EMPTY_LIST(hd)) {
        argv = erlxc_list_to_argv(hd);
        if (!argv)
            goto BADARG;
    }

    if (!lxc_container_get(c))
        goto BADARG;

    switch (fork()) {
        case -1:
            res = false;
            break;
        case 0:
            if (prctl(PR_SET_PDEATHSIG, SIGKILL) < 0)
                erl_err_sys("signal");

            res = c->start(c, useinit, argv);

            erlxc_lxc_container_put(ep);

            (void)erlxc_send(erlxc_tuple2(erl_mk_atom("start"),
                        erlxc_bool(res)));
            exit (0);
        default:
            res = true;
            break;
    }

    erlxc_lxc_container_put(ep);
    erlxc_free_argv(argv);
    return erlxc_bool(res);

BADARG:
    erlxc_free_argv(argv);
    return erl_mk_atom("badarg");
}

    static ETERM *
erlxc_lxc_container_stop(erlxc_state_t *ep, ETERM *arg)
{
    struct lxc_container *c = ep->c;

    if (!c)
        return erl_mk_atom("badarg");

    return erlxc_bool(c->stop(c));
}

    static ETERM *
erlxc_lxc_container_shutdown(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    struct lxc_container *c = ep->c;
    int timeout = -1;

    if (!c)
        return erl_mk_atom("badarg");

    /* timeout */
    arg = erlxc_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    timeout = ERL_INT_VALUE(hd);
    if (timeout < 0)
        goto BADARG;

    return erlxc_bool(c->shutdown(c, timeout));

BADARG:
    return erl_mk_atom("badarg");
}

    static ETERM *
erlxc_lxc_container_init_pid(erlxc_state_t *ep, ETERM *arg)
{
    struct lxc_container *c = ep->c;
    pid_t pid = -1;

    if (!c)
        return erl_mk_atom("badarg");

    pid = c->init_pid(c);

    /* XXX overflow pid_t -> int */
    return erl_mk_int(pid);
}

    static ETERM *
erlxc_lxc_container_load_config(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    struct lxc_container *c = ep->c;
    char *path = NULL;

    if (!c)
        return erl_mk_atom("badarg");

    arg = erlxc_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    if (erl_iolist_length(hd) > 0) {
        path = erl_iolist_to_string(hd);
        if (!path)
            goto BADARG;
    }

    return erlxc_bool(c->load_config(c, path));

BADARG:
    return erl_mk_atom("badarg");
}

    static ETERM *
erlxc_lxc_container_save_config(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    struct lxc_container *c = ep->c;
    char *path = NULL;

    if (!c)
        return erl_mk_atom("badarg");

    arg = erlxc_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        path = erl_iolist_to_string(hd);

    if (!path)
        goto BADARG;

    return erlxc_bool(c->save_config(c, path));

BADARG:
    return erl_mk_atom("badarg");
}

    static ETERM *
erlxc_lxc_container_get_keys(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    struct lxc_container *c = ep->c;
    char *key = NULL;
    char buf[2048] = {0};
    int n = 0;
    int len = 0;

    if (!c)
        return erl_mk_atom("badarg");

    arg = erlxc_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    if (erl_iolist_length(hd) > 0) {
        key = erl_iolist_to_string(hd);
        if (!key)
            goto BADARG;
    }

    if (!key)
        len = c->get_keys(c, NULL, NULL, 0);

    /* XXX better error if length is too large */
    if (len < 0 || len >= sizeof(buf))
        goto BADARG;

    n = c->get_keys(c, key, buf, (key ? sizeof(buf) : len+1));

    return (n > 0 ? erl_mk_binary(buf, n) : erl_mk_binary("",0));

BADARG:
    erl_free(key);

    return erl_mk_atom("badarg");
}

    static ETERM *
erlxc_lxc_container_config_file_name(erlxc_state_t *ep, ETERM *arg)
{
    struct lxc_container *c = ep->c;
    char *name = NULL;

    if (!c)
        return erl_mk_atom("badarg");

    name = c->config_file_name(c);
    if (!name)
        return erl_mk_binary("",0);

    free(name);

    return erlxc_bin(name);
}

    static ETERM *
erlxc_lxc_container_clear_config(erlxc_state_t *ep, ETERM *arg)
{
    struct lxc_container *c = ep->c;

    if (!c)
        return erl_mk_atom("badarg");

    c->clear_config(c);
    return erl_mk_atom("true");
}

    static ETERM *
erlxc_lxc_container_clear_config_item(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    struct lxc_container *c = ep->c;
    char *key = NULL;
    bool res;

    if (!c)
        return erl_mk_atom("badarg");

    /* key */
    arg = erlxc_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        key = erl_iolist_to_string(hd);

    if (!key)
        goto BADARG;

    res = c->clear_config_item(c, key);

    erl_free(key);

    return erlxc_bool(res);

BADARG:
    erl_free(key);

    return erl_mk_atom("badarg");
}

    static ETERM *
erlxc_lxc_container_get_config_item(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    struct lxc_container *c = ep->c;
    char *key = NULL;
    char *buf = NULL;
    int n = 0;
    ETERM *t = NULL;

    if (!c)
        return erl_mk_atom("badarg");

    /* key */
    arg = erlxc_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        key = erl_iolist_to_string(hd);

    if (!key)
        goto BADARG;

    n = c->get_config_item(c, key, NULL, 0);

    /* 0 is ??? */
    if (n < 1) {
        erl_free(key);
        return erl_mk_atom("none");
    }

    /* account for null */
    buf = erl_malloc(n+1);
    if (!buf)
        goto BADARG;

    (void)c->get_config_item(c, key, buf, n+1);

    /* null is not included in binary */
    t = erl_mk_binary(buf, n);

    erl_free(key);
    erl_free(buf);

    return t;

BADARG:
    erl_free(key);
    erl_free(buf);

    return erl_mk_atom("badarg");
}

    static ETERM *
erlxc_lxc_container_set_config_item(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    struct lxc_container *c = ep->c;
    char *key = NULL;
    char *val = NULL;
    bool res;

    if (!c)
        return erl_mk_atom("badarg");

    /* key */
    arg = erlxc_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        key = erl_iolist_to_string(hd);

    if (!key)
        goto BADARG;

    /* value */
    arg = erlxc_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    if (erl_iolist_length(hd) > 0) {
        val = erl_iolist_to_string(hd);
        if (!val)
            goto BADARG;
    }

    res = c->set_config_item(c, key, val);

    erl_free(key);
    erl_free(val);

    return erlxc_bool(res);

BADARG:
    erl_free(key);
    erl_free(val);

    return erl_mk_atom("badarg");
}

    static ETERM *
erlxc_lxc_container_get_config_path(erlxc_state_t *ep, ETERM *arg)
{
    struct lxc_container *c = ep->c;
    const char *path = NULL;

    if (!c)
        return erl_mk_atom("badarg");

    path = c->get_config_path(c);

    if (!path)
        return erl_mk_binary("",0);

    return erlxc_bin(path);
}

    static ETERM *
erlxc_lxc_container_set_config_path(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    struct lxc_container *c = ep->c;
    char *path = NULL;
    bool res;

    if (!c)
        return erl_mk_atom("badarg");

    /* path */
    arg = erlxc_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        path = erl_iolist_to_string(hd);

    if (!path)
        goto BADARG;

    res = c->set_config_path(c, path);
    erl_free(path);
    return erlxc_bool(res);

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

    return erl_mk_list(reply, n);

BADARG:
    return erl_mk_atom("badarg");
}

    char **
erlxc_list_to_argv(ETERM *arg)
{
    ETERM *hd = NULL;
    int len = 0; /* XXX overflow */
    int i = 0;
    char **argv = NULL;

    len = erl_length(arg);

    /* NULL terminate */
    argv = calloc(len + 1, sizeof(char **));

    if (!argv)
        return NULL;

    for (i = 0; i < len; i++) {
        arg = erlxc_list_head(&hd, arg);
        if (!hd)
            return NULL;

        argv[i] = erl_iolist_to_string(hd);
        if (!argv[i])
            return NULL;
    }

    return argv;
}

    static void
erlxc_free_argv(char **argv)
{
    int i = 0;

    if (argv == NULL)
        return;

    for (i = 0; argv[i] != NULL; i++)
        erl_free(argv[i]);
}

    void
erlxc_lxc_container_put(erlxc_state_t *ep)
{
    if (lxc_container_put(ep->c) == 1)
        ep->c = NULL;

    return;
}

/*
 * For tests only
 */
    static ETERM *
erlxc_test_argv(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    char **argv = NULL;
    int len = 0;
    int i = 0;

    arg = erlxc_list_head(&hd, arg);
    if (!hd)
        return erl_mk_atom("badarg");

    len = erl_length(hd);
    VERBOSE(0, "len=%d", len);

    argv = erlxc_list_to_argv(hd);

    if (!argv)
        return erl_mk_atom("badarg");

    for (i = 0; i < len; i++)
        (void)fprintf(stderr, "arg[%d]=%s", i, argv[i]);

    return erlxc_bool(true);
}
