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
#include <lxc/lxccontainer.h>
#include <lxc/lxc.h>

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <err.h>

#include <erl_nif.h>
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

#define MAXBUFLEN       65535
#define ERLXC_VERSION   "0.1.0"

#define LXCMAX  128
#define LXCNAME "erlxc"
#define LXCPATH "/var/lib/lxc"

typedef struct {
    u_int32_t cur;
    u_int32_t max;
    u_int8_t verbose;
    struct lxc_container **c;
    //erlxc_lxc_t *c;
} erlxc_state_t;

typedef struct {
    u_int32_t cmd;
    unsigned char *arg;
} erlxc_msg_t;

ETERM *erlxc_list_head(ETERM **, ETERM *);
ETERM *erlxc_tuple2(ETERM *, ETERM *);
ETERM *erlxc_tuple3(ETERM *, ETERM *, ETERM *);
ETERM *erlxc_error(const char *);
ETERM *erlxc_ok(ETERM *);
ETERM *erlxc_errno(int);

ETERM *erlxc_cmd(erlxc_state_t *, u_int32_t, ETERM *);
ETERM *erlxc_lxc_container_new(erlxc_state_t *, ETERM *);
ETERM *erlxc_lxc_container_start(erlxc_state_t *, ETERM *);
ETERM *erlxc_lxc_container_stop(erlxc_state_t *, ETERM *);
ETERM *erlxc_list_containers(erlxc_state_t *, ETERM *,
        int (*)(const char *, char ***, struct lxc_container ***));
ETERM *erlxc_lxc_container_load_config(erlxc_state_t *, ETERM *);

struct lxc_container *erlxc_cid(erlxc_state_t *, int);

#define VERBOSE(x, ...) do { \
    if (ep->verbose >= x) { \
        erl_err_msg(__VA_ARGS__); \
    } \
} while (0)
