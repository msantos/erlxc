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

    ETERM *
erlxc_tuple2(ETERM *tag, ETERM *term)
{
    ETERM *t[2] = {tag, term};

    return erl_mk_tuple(t, 2);
}

    ETERM *
erlxc_tuple3(ETERM *t1, ETERM *t2, ETERM *t3)
{
    ETERM *t[3] = {t1, t2, t3};

    return erl_mk_tuple(t, 3);
}

    ETERM *
erlxc_errno(int errnum)
{
    return erlxc_error(erl_errno_id(errnum));
}

    ETERM *
erlxc_error(const char *reason)
{
    return erlxc_tuple2(erl_mk_atom("error"), erl_mk_atom(reason));
}

    ETERM *
erlxc_ok(ETERM *term)
{
    return erlxc_tuple2(erl_mk_atom("ok"), term);
}

    ETERM *
erlxc_bool(bool ok)
{
    return (ok ? erl_mk_atom("true") : erl_mk_atom("false"));
}

    ETERM *
erlxc_bin(const char *buf)
{
    return (buf ? erl_mk_binary(buf, strlen(buf)) : erl_mk_binary("",0));
}

    ETERM *
erlxc_list_head(ETERM **hd, ETERM *list)
{
    *hd = erl_hd(list);
    return erl_tl(list);
}

    void *
erlxc_malloc(ssize_t size)
{
    void *buf = NULL;

    if (size < 0 || size >= INT32_MAX)
        erl_err_quit("malloc:invalid size:%ld", size);

    buf = erl_malloc(size);

    if (!buf)
        erl_err_quit("malloc");

    return buf;
}
