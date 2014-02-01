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
#include <lxc/lxccontainer.h>
#include <lxc/lxc.h>

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <err.h>

#include <erl_driver.h>
#include <erl_interface.h>
#include <ei.h>

#include <unistd.h>
#include <signal.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <stdlib.h>

#include <sys/param.h>

enum {
    erlxc_opt_stop_on_exit = 1 << 0,        /* Stop the container when the port exits */
    erlxc_opt_destroy_on_exit = 1 << 1,     /* Destroy the container when the port exits (must be stopped) */
    erlxc_opt_daemonize = 1 << 2,           /* Debug: allows disabling daemonize mode */
    erlxc_opt_closeallfds = 1 << 3,         /* Debug: allows disabling closeallfds mode */
};

typedef struct {
    u_int32_t opt;
    u_int8_t verbose;
    struct lxc_container *c;
} erlxc_state_t;

typedef struct {
    u_int32_t cmd;
    unsigned char *arg;
} erlxc_msg_t;

int erlxc_send(ETERM *t);

void *erlxc_malloc(ssize_t size);

ETERM *erlxc_list_head(ETERM **, ETERM *);
ETERM *erlxc_tuple2(ETERM *, ETERM *);
ETERM *erlxc_tuple3(ETERM *, ETERM *, ETERM *);
ETERM *erlxc_error(const char *);
ETERM *erlxc_ok(ETERM *);
ETERM *erlxc_bool(bool);
ETERM *erlxc_bin(const char *);

ETERM *erlxc_cmd(erlxc_state_t *, u_int32_t, ETERM *);

#define VERBOSE(x, ...) do { \
    if (ep->verbose >= x) { \
        erl_err_msg(__VA_ARGS__); \
    } \
} while (0)
