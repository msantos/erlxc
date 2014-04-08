/* Copyright (c) 2013-2014, Michael Santos <michael.santos@gmail.com>
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
#include "erlxc_cmd.h"

static char **erlxc_list_to_argv(ETERM *);
static void erlxc_free_argv(char **);
static ETERM *erlxc_list_containers(erlxc_state_t *, ETERM *,
        int (*)(const char *, char ***, struct lxc_container ***));

#define ERLXC_IS_IOLIST(_t)  (ERL_IS_BINARY(_t) || ERL_IS_LIST(_t))

    ETERM *
erlxc_cmd(erlxc_state_t *ep, u_int32_t cmd, ETERM *arg)
{
    erlxc_cmd_t *fun = NULL;

    VERBOSE(3, "cmd=%d", cmd);
    if (ep->verbose >= 3)
        erl_print_term(stderr, arg);

    if (cmd >= sizeof(cmds)/sizeof(cmds[0]))
        return erl_mk_atom("badarg");

    fun = &cmds[cmd];

    if (!ERL_IS_LIST(arg) || erl_length(arg) != fun->narg)
        return erl_mk_atom("badarg");

    return (*fun->fp)(ep, arg);
}

    static ETERM *
erlxc_version(erlxc_state_t *ep, ETERM *arg)
{
    return erlxc_bin(lxc_get_version());
}

    static ETERM *
erlxc_lxc_container_name(erlxc_state_t *ep, ETERM *arg)
{
    return erlxc_bin(ep->c->name);
}

    static ETERM *
erlxc_lxc_container_rename(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    struct lxc_container *c = ep->c;
    char *newname = NULL;
    bool res;

    /* name */
    arg = erlxc_list_head(&hd, arg);
    if (!hd || !ERLXC_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        newname = erl_iolist_to_string(hd);

    if (!newname)
        goto BADARG;

    res = c->rename(c, newname);

    if (res) {
        ep->c = lxc_container_new(newname, c->get_config_path(c));
        if (!ep->c)
            erl_err_quit("failed to create container");

        (void)lxc_container_put(c);
    }

    erl_free(newname);

    return erlxc_bool(res);

BADARG:
    return erl_mk_atom("badarg");
}

    static ETERM *
erlxc_lxc_container_state(erlxc_state_t *ep, ETERM *arg)
{
    return erlxc_bin(ep->c->state(ep->c));
}

    static ETERM *
erlxc_lxc_container_wait(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    struct lxc_container *c = ep->c;
    char *state = NULL;
    int timeout = 0;
    bool res;

    /* state */
    arg = erlxc_list_head(&hd, arg);
    if (!hd || !ERLXC_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        state = erl_iolist_to_string(hd);

    if (!state)
        goto BADARG;

    /* timeout */
    arg = erlxc_list_head(&hd, arg);
    if (!hd || !ERL_IS_INTEGER(hd))
        goto BADARG;

    timeout = ERL_INT_VALUE(hd);
    if (timeout < -1)
        goto BADARG;

    res = c->wait(c, state, timeout);

    erl_free(state);

    return erlxc_bool(res);

BADARG:
    erl_free(state);
    return erl_mk_atom("badarg");
}

    static ETERM *
erlxc_lxc_container_defined(erlxc_state_t *ep, ETERM *arg)
{
    return erlxc_bool(ep->c->is_defined(ep->c));
}

    static ETERM *
erlxc_lxc_container_running(erlxc_state_t *ep, ETERM *arg)
{
    return erlxc_bool(ep->c->is_running(ep->c));
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

    /* template */
    arg = erlxc_list_head(&hd, arg);
    if (!hd || !ERLXC_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        t = erl_iolist_to_string(hd);

    /* bdevtype */
    arg = erlxc_list_head(&hd, arg);
    if (!hd || !ERLXC_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        bdevtype = erl_iolist_to_string(hd);

    /* specs */
    arg = erlxc_list_head(&hd, arg);
    if (!hd)
        goto BADARG;

    /* flags */
    arg = erlxc_list_head(&hd, arg);
    if (!hd || !ERL_IS_INTEGER(hd))
        goto BADARG;

    flags = ERL_INT_VALUE(hd);
    flags |= LXC_CREATE_QUIET;

    /* argv */
    arg = erlxc_list_head(&hd, arg);
    if (!hd || !ERL_IS_LIST(hd))
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

    erl_free(t);
    erl_free(bdevtype);
    erlxc_free_argv(argv);

    return erlxc_bool(res);

BADARG:
    erl_free(t);
    erl_free(bdevtype);
    erlxc_free_argv(argv);
    return erl_mk_atom("badarg");
}

    static ETERM *
erlxc_lxc_container_destroy(erlxc_state_t *ep, ETERM *arg)
{
    return erlxc_bool(ep->c->destroy(ep->c));
}

    static ETERM *
erlxc_lxc_container_start(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    struct lxc_container *c = ep->c;
    int useinit = 0;
    char **argv = NULL;
    bool res;

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

    if (!c->want_daemonize(c, (ep->opt & erlxc_opt_daemonize ? true : false)))
        goto BADARG;

    if (!c->want_close_all_fds(c, (ep->opt & erlxc_opt_closeallfds ? true : false)))
        goto BADARG;

    res = c->start(c, useinit, argv);

    erlxc_free_argv(argv);
    return erlxc_bool(res);

BADARG:
    erlxc_free_argv(argv);
    return erl_mk_atom("badarg");
}

    static ETERM *
erlxc_lxc_container_stop(erlxc_state_t *ep, ETERM *arg)
{
    return erlxc_bool(ep->c->stop(ep->c));
}

    static ETERM *
erlxc_lxc_container_shutdown(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    struct lxc_container *c = ep->c;
    int timeout = -1;

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
erlxc_lxc_container_reboot(erlxc_state_t *ep, ETERM *arg)
{
    return erlxc_bool(ep->c->reboot(ep->c));
}

    static ETERM *
erlxc_lxc_container_freeze(erlxc_state_t *ep, ETERM *arg)
{
    return erlxc_bool(ep->c->freeze(ep->c));
}

    static ETERM *
erlxc_lxc_container_unfreeze(erlxc_state_t *ep, ETERM *arg)
{
    return erlxc_bool(ep->c->unfreeze(ep->c));
}

    static ETERM *
erlxc_lxc_container_init_pid(erlxc_state_t *ep, ETERM *arg)
{
    return erl_mk_longlong(ep->c->init_pid(ep->c));
}

    static ETERM *
erlxc_lxc_container_load_config(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    struct lxc_container *c = ep->c;
    char *path = NULL;
    bool res;

    arg = erlxc_list_head(&hd, arg);
    if (!hd || !ERLXC_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0) {
        path = erl_iolist_to_string(hd);
        if (!path)
            goto BADARG;
    }

    res = c->load_config(c, path);

    erl_free(path);

    return erlxc_bool(res);

BADARG:
    erl_free(path);
    return erl_mk_atom("badarg");
}

    static ETERM *
erlxc_lxc_container_save_config(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    struct lxc_container *c = ep->c;
    char *path = NULL;
    bool res;

    arg = erlxc_list_head(&hd, arg);
    if (!hd || !ERLXC_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        path = erl_iolist_to_string(hd);

    if (!path)
        goto BADARG;

    res = c->save_config(c, path);

    erl_free(path);

    return erlxc_bool(res);

BADARG:
    erl_free(path);
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

    arg = erlxc_list_head(&hd, arg);
    if (!hd || !ERLXC_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0) {
        key = erl_iolist_to_string(hd);
        if (!key)
            goto BADARG;
    }

    if (!key)
        len = c->get_keys(c, NULL, NULL, 0);

    if (len < 0 || len >= sizeof(buf))
        goto BADARG;

    n = c->get_keys(c, key, buf, (key ? sizeof(buf) : len+1));

    erl_free(key);

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
    ETERM *t = NULL;

    name = c->config_file_name(c);
    t = erlxc_bin(name);
    free(name);
    return t;
}

    static ETERM *
erlxc_lxc_container_clear_config(erlxc_state_t *ep, ETERM *arg)
{
    ep->c->clear_config(ep->c);
    return erlxc_bool(true);
}

    static ETERM *
erlxc_lxc_container_clear_config_item(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    struct lxc_container *c = ep->c;
    char *key = NULL;
    bool res;

    /* key */
    arg = erlxc_list_head(&hd, arg);
    if (!hd || !ERLXC_IS_IOLIST(hd))
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

    /* key */
    arg = erlxc_list_head(&hd, arg);
    if (!hd || !ERLXC_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        key = erl_iolist_to_string(hd);

    if (!key)
        goto BADARG;

    n = c->get_config_item(c, key, NULL, 0);

    if (n < 0) {
        erl_free(key);
        return erl_mk_atom("false");
    }
    else if (n == 0) {
        erl_free(key);
        return erl_mk_binary("",0);
    }

    /* account for null */
    buf = erlxc_malloc(n+1);

    (void)c->get_config_item(c, key, buf, n+1);

    /* null is not included in binary */
    t = erl_mk_binary(buf, n);

    erl_free(key);
    erl_free(buf);

    return t;

BADARG:
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

    /* key */
    arg = erlxc_list_head(&hd, arg);
    if (!hd || !ERLXC_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        key = erl_iolist_to_string(hd);

    if (!key)
        goto BADARG;

    /* value */
    arg = erlxc_list_head(&hd, arg);
    if (!hd || !ERLXC_IS_IOLIST(hd))
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
    return erlxc_bin(ep->c->get_config_path(ep->c));
}

    static ETERM *
erlxc_lxc_container_set_config_path(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    struct lxc_container *c = ep->c;
    char *path = NULL;
    bool res;

    /* path */
    arg = erlxc_list_head(&hd, arg);
    if (!hd || !ERLXC_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        path = erl_iolist_to_string(hd);

    if (!path)
        goto BADARG;

    res = c->set_config_path(c, path);
    erl_free(path);
    return erlxc_bool(res);

BADARG:
    erl_free(path);
    return erl_mk_atom("badarg");
}

    static ETERM *
erlxc_lxc_container_get_cgroup_item(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    struct lxc_container *c = ep->c;
    char *key = NULL;
    char *buf = NULL;
    int n = 0;
    ETERM *t = NULL;

    /* key */
    arg = erlxc_list_head(&hd, arg);
    if (!hd || !ERLXC_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        key = erl_iolist_to_string(hd);

    if (!key)
        goto BADARG;

    n = c->get_cgroup_item(c, key, NULL, 0);

    if (n < 0) {
        erl_free(key);
        return erl_mk_atom("false");
    }
    else if (n == 0) {
        erl_free(key);
        return erl_mk_binary("",0);
    }

    /* account for null */
    buf = erlxc_malloc(n+1);

    (void)c->get_cgroup_item(c, key, buf, n+1);

    /* null is not included in binary */
    t = erl_mk_binary(buf, n);

    erl_free(key);
    erl_free(buf);

    return t;

BADARG:
    return erl_mk_atom("badarg");
}

    static ETERM *
erlxc_lxc_container_set_cgroup_item(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    struct lxc_container *c = ep->c;
    char *key = NULL;
    char *val = NULL;
    bool res;

    /* key */
    arg = erlxc_list_head(&hd, arg);
    if (!hd || !ERLXC_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        key = erl_iolist_to_string(hd);

    if (!key)
        goto BADARG;

    /* value */
    arg = erlxc_list_head(&hd, arg);
    if (!hd || !ERLXC_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0) {
        val = erl_iolist_to_string(hd);
        if (!val)
            goto BADARG;
    }

    res = c->set_cgroup_item(c, key, val);

    erl_free(key);
    erl_free(val);

    return erlxc_bool(res);

BADARG:
    erl_free(key);
    erl_free(val);

    return erl_mk_atom("badarg");
}

    static ETERM *
erlxc_lxc_container_get_interfaces(erlxc_state_t *ep, ETERM *arg)
{
    char **iface = NULL;
    ETERM *t = NULL;
    int i = 0;

    t = erl_mk_empty_list();

    iface = ep->c->get_interfaces(ep->c);

    if (!iface)
        return t;

    for (i = 0; iface[i]; i++) {
        t = erl_cons(erl_mk_binary(iface[i], strlen(iface[i])), t);
        free(iface[i]);
    }

    free(iface);
    return t;
}

    static ETERM *
erlxc_lxc_container_get_ips(erlxc_state_t *ep, ETERM *arg)
{
    ETERM *hd = NULL;
    char **ips = NULL;
    const char *interface = NULL;
    const char *family = NULL;
    int scope = 0;
    ETERM *t = NULL;
    int i = 0;

    /* interface */
    arg = erlxc_list_head(&hd, arg);
    if (!hd || !ERLXC_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        interface = erl_iolist_to_string(hd);

    /* family */
    arg = erlxc_list_head(&hd, arg);
    if (!hd || !ERLXC_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0)
        family = erl_iolist_to_string(hd);

    /* scope */
    arg = erlxc_list_head(&hd, arg);
    if (!hd || !ERL_IS_INTEGER(hd))
        goto BADARG;

    scope = ERL_INT_VALUE(hd);

    t = erl_mk_empty_list();

    ips = ep->c->get_ips(ep->c, interface, family, scope);

    if (!ips)
        return t;

    for (i = 0; ips[i]; i++) {
        t = erl_cons(erl_mk_binary(ips[i], strlen(ips[i])), t);
        free(ips[i]);
    }

    free(ips);
    return t;

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
    ETERM *t = NULL;

    arg = erlxc_list_head(&hd, arg);
    if (!hd || !ERLXC_IS_IOLIST(hd))
        goto BADARG;

    if (erl_iolist_length(hd) > 0) {
        path = erl_iolist_to_string(hd);
        if (!path)
            goto BADARG;
    }

    n = fun(path, &names, NULL);

    erl_free(path);
    path = NULL;

    if (n < 0)
        goto BADARG;

    t = erl_mk_empty_list();
    for (i = 0; i < n; i++) {
        t = erl_cons(erl_mk_binary(names[i], strlen(names[i])), t);
        free(names[i]);
    }

    if (n > 0)
        free(names);

    return t;

BADARG:
    erl_free(path);
    return erl_mk_atom("badarg");
}

    char **
erlxc_list_to_argv(ETERM *arg)
{
    ETERM *hd = NULL;
    ssize_t len = 0;
    int i = 0;
    char **argv = NULL;
    long maxarg = sysconf(_SC_ARG_MAX);

    len = erl_length(arg);

    if (len < 0 || len >= maxarg)
        return NULL;

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

    for (i = 0; argv[i]; i++)
        free(argv[i]);

    free(argv);
}

/*
 * Set erlxc state
 */
    static ETERM *
erlxc_type(erlxc_state_t *ep, ETERM *arg)
{
    if ( (ep->opt & erlxc_opt_stop_on_exit) &&
            (ep->opt & erlxc_opt_destroy_on_exit))
        return erl_mk_atom("temporary");

    if (ep->opt & erlxc_opt_stop_on_exit)
        return erl_mk_atom("transient");

    return erl_mk_atom("permanent");
}

    static ETERM *
erlxc_temporary(erlxc_state_t *ep, ETERM *arg)
{
    ep->opt |= erlxc_opt_stop_on_exit;
    ep->opt |= erlxc_opt_destroy_on_exit;
    return erlxc_bool(true);
}

    static ETERM *
erlxc_transient(erlxc_state_t *ep, ETERM *arg)
{
    ep->opt |= erlxc_opt_stop_on_exit;
    ep->opt &= ~erlxc_opt_destroy_on_exit;
    return erlxc_bool(true);
}

    static ETERM *
erlxc_permanent(erlxc_state_t *ep, ETERM *arg)
{
    ep->opt &= ~erlxc_opt_stop_on_exit;
    ep->opt &= ~erlxc_opt_destroy_on_exit;
    return erlxc_bool(true);
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
    ETERM *t = NULL;

    arg = erlxc_list_head(&hd, arg);
    if (!hd)
        return erl_mk_atom("badarg");

    len = erl_length(hd);
    VERBOSE(3, "len=%d", len);

    argv = erlxc_list_to_argv(hd);

    if (!argv)
        return erl_mk_atom("badarg");

    t = erl_mk_empty_list();
    for (i = 0; i < len; i++) {
        VERBOSE(3, "arg[%d]=%s", i, argv[i]);
        t = erl_cons(erl_mk_binary(argv[i], strlen(argv[i])), t);
    }

    erlxc_free_argv(argv);
    return t;
}
