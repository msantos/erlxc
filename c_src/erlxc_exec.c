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
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <limits.h>

#include <err.h>
#include <errno.h>

typedef union {
    uid_t u;
    gid_t g;
    unsigned long long ul;
} execid_t;

int argtoul(char *, unsigned long long *);

    int
main(int argc, char *argv[])
{
    execid_t uid = {0};
    execid_t gid = {0};

    if (argc < 4)
        errx(EXIT_FAILURE, "<uid> <gid> <cmd> <arg> <...>");

    if ( (argtoul(argv[2], &gid.ul) < 0) || (setgid(gid.g) < 0))
        err(2, "setgid");

    if ( (argtoul(argv[1], &uid.ul) < 0) || (setuid(uid.u) < 0))
        err(2, "setuid");

    execvp(argv[3], &argv[3]);
    err(1, "execvp");
}

    int
argtoul(char *str, unsigned long long *val)
{
    if (!str) {
        errno = EINVAL;
        return -1;
    }

    errno = 0;
    *val = strtoll(str, (char **)NULL, 10);

    if (errno != 0)
        return -1;

    if (*val > UINT_MAX) {
        errno = ERANGE;
        return -1;
    }

    return 0;
}
