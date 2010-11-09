/*
***** BEGIN LICENSE BLOCK *****

This file is part of the EPI (Erlang Plus Interface) Library.

Copyright (C) 2005 Hector Rivas Gandara <keymon@gmail.com>

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

***** END LICENSE BLOCK *****
*/

/*
 * Implementation format functionality
 * This is *a copy* of source from erl_interface
 *
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 */

#include "Config.hpp" // Main config file

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <iostream>

#include "ErlTypes.hpp"

using namespace epi::type;
using namespace epi::error;

// FIXME: Write my own format code X-)
namespace epi {
namespace parse {

#define ERL_OK 0
#define ERL_FORMAT_ERROR -1

#define ERL_MAX_ENTRIES 255      /* Max entries in a tuple/list term */
#define ERL_MAX_NAME_LENGTH 255  /* Max length of variable names */


/* Forward */
static ErlTerm *eformat(char**, va_list*);

static void skip_null_chars(char **fmt) {

    char c = *(*fmt);

    while (c == ' ' || c == '\t' || c == '\n') {
        (*fmt)++;
        c =  **fmt;
    }

}

#define erl_err_msg(msg) std::cout << msg << "\n"

static char *pvariable(char **fmt, char *buf)
{
    char *start=*fmt;
    char c;
    int len;

    skip_null_chars(fmt);

    while (1) {
        c = *(*fmt)++;
        if (isalnum((int) c) || (c == '_'))
            continue;
        else
            break;
    }
    (*fmt)--;
    len = *fmt - start;
    memcpy(buf, start, len);
    buf[len] = 0;

    return buf;

} /* pvariable */

static char *patom(char **fmt, char *buf)
{
    char *start=*fmt;
    char c;
    int len;

    skip_null_chars(fmt);

    while (1) {
        c = *(*fmt)++;
        if (isalnum((int) c) || (c == '_') || (c == '@'))
            continue;
        else
            break;
    }
    (*fmt)--;
    len = *fmt - start;
    memcpy(buf, start, len);
    buf[len] = 0;

    return buf;

} /* patom */

/* Check if integer or float
 */
static char *pdigit(char **fmt, char *buf)
{
    char *start=*fmt;
    char c;
    int len,dotp=0;

    skip_null_chars(fmt);

    while (1) {
        c = *(*fmt)++;
        if (isdigit((int) c) || c == '-')
            continue;
        else if (!dotp && (c == '.')) {
            dotp = 1;
            continue;
        }
        else
            break;
    }
    (*fmt)--;
    len = *fmt - start;
    memcpy(buf, start, len);
    buf[len] = 0;

    return buf;

} /* pdigit */

static char *pstring(char **fmt, char *buf)
{
    char *start=++(*fmt); /* skip first quote */
    char c;
    int len;

    skip_null_chars(fmt);

    while (1) {
        c = *(*fmt)++;
        if (c == '"') {
            if (*((*fmt)-1) == '\\')
                continue;
            else
                break;
        } else
            continue;
    }
    len = *fmt - 1 - start; /* skip last quote */
    memcpy(buf, start, len);
    buf[len] = 0;

    return buf;

} /* pstring */

static char *pquotedatom(char **fmt, char *buf)
{
    char *start=++(*fmt); /* skip first quote */
    char c;
    int len;

    skip_null_chars(fmt);

    while (1) {
        c = *(*fmt)++;
        if (c == '\'') {
            if (*((*fmt)-1) == '\\')
                continue;
            else
                break;
        } else
            continue;
    }
    len = *fmt - 1 - start; /* skip last quote */
    memcpy(buf, start, len);
    buf[len] = 0;

    return buf;

} /* pquotedatom */


/*
 * The format letters are:
 *   w  -  Any Erlang term
 *   a  -  An Atom
 *   s  -  A String
 *   i  -  An Integer
 *   f  -  A Float (double)
 */
static int pformat(char **fmt, va_list *pap, ErlTerm *v[], int size)
{
    int rc=ERL_OK;

    /* this next section hacked to remove the va_arg calls */
    skip_null_chars(fmt);

    switch (*(*fmt)++) {

        case 'w':
            v[size] = va_arg(*pap, ErlTerm*);
            break;

        case 'a':
            v[size] = new ErlAtom(va_arg(*pap, char *));
            break;

        case 's':
            v[size] = new ErlString(va_arg(*pap, char *));
            break;

        case 'i':
            v[size] = new ErlLong(va_arg(*pap, int));
            break;

        case 'f':
            v[size] = new ErlDouble(va_arg(*pap, double));
            break;

        default:
            rc = ERL_FORMAT_ERROR;
            break;
    }

    return rc;

} /* pformat */

static int ptuple(char **fmt, va_list *pap, ErlTerm *v[], int size)
{
    int res=ERL_FORMAT_ERROR;

    skip_null_chars(fmt);

    switch (*(*fmt)++) {

        case '}':
            res = size;
            break;

        case ',':
            res = ptuple(fmt, pap, v, size);
            break;

            default: {
                (*fmt)--;
                if ((v[size++] = eformat(fmt, pap)) != (ErlTerm *) NULL)
                    res = ptuple(fmt, pap, v, size);
                else
                    erl_err_msg("ptuple(1):  Invalid recursion !");
                break;

      /*
                if (isupper(**fmt)) {
                v[size++] = erl_mk_var(pvariable(fmt, wbuf));
                res = ptuple(fmt, pap, v, size);
            }
                else if ((v[size++] = eformat(fmt, pap)) != (ErlTerm *) NULL)
                res = ptuple(fmt, pap, v, size);
                break;
      */
            }

    } /* switch */

    return res;

} /* ptuple */


static int plist(char **fmt, va_list *pap, ErlTerm *v[], int size)
{
    int res=ERL_FORMAT_ERROR;

    skip_null_chars(fmt);

    switch (*(*fmt)++) {

        case ']':
            res = size;
            break;

        case ',':
            res = plist(fmt, pap, v, size);
            break;

            default: {
                (*fmt)--;
                if ((v[size++] = eformat(fmt, pap)) != (ErlTerm *) NULL)
                    res = plist(fmt, pap, v, size);
                break;

      /*
                if (isupper(**fmt)) {
                v[size++] = erl_mk_var(pvariable(fmt, wbuf));
                res = plist(fmt, pap, v, size);
            }
                else if ((v[size++] = eformat(fmt, pap)) != (ErlTerm *) NULL)
                res = plist(fmt, pap, v, size);
                break;
      */
            }

    } /* switch */

    return res;

} /* plist */


static ErlTerm *eformat(char **fmt, va_list *pap)
{
    int size;
    // FIXME: Allow dinamic growing
    ErlTerm *v[ERL_MAX_ENTRIES], *returnTerm = 0;

    skip_null_chars(fmt);

    switch (*(*fmt)++) {
        case '{':
            if ((size = ptuple(fmt, pap , v, 0)) != ERL_FORMAT_ERROR) {
                returnTerm = new ErlTuple(v, size);
            }
            break;

        case '[':
            if (**fmt == ']') {
                (*fmt)++;
                returnTerm = new ErlEmptyList();
            } else if ((size = plist(fmt, pap , v, 0)) != ERL_FORMAT_ERROR) {
                returnTerm = new ErlConsList(v, size);
            }
            break;

            case '$': /* char-value? */
                returnTerm = new ErlLong((int)(*(*fmt)++));
                break;
        case '~':
            if (pformat(fmt, pap, v, 0) == ERL_OK) {
                returnTerm = v[0];
            }
            break;

    /* handle negative numbers too...
     * case '-':
            * {
            * ErlTerm *tmp;
            *
            * tmp = eformat(fmt,pap);
            *     if (ERL_IS_INTEGER(tmp)) ERL_INT_VALUE(tmp) = -(ERL_INT_VALUE(tmp));
            * return tmp;
            * }
            *
            *
            * break;
    */

        default:
        {
            char wbuf[BUFSIZ];  /* now local to this function for reentrancy */

            (*fmt)--;
            if (islower((int)**fmt)) {         /* atom  ? */
                char *atom=patom(fmt, wbuf);
                returnTerm = new ErlAtom(atom);
            }
            else if (isupper((int)**fmt) || (**fmt == '_')) {
                char *var=pvariable(fmt, wbuf);
                returnTerm = new ErlVariable(var);
            }
            else if (isdigit((int)**fmt) || **fmt == '-') {    /* integer/float ? */
                char *digit=pdigit(fmt, wbuf);
                if (strchr(digit,(int) '.') == NULL)
                    returnTerm = new ErlLong(atoi((const char *) digit));
                else
                    returnTerm = new ErlDouble(atof((const char *) digit));
            }
            else if (**fmt == '"') {      /* string ? */
                char *string=pstring(fmt, wbuf);
                returnTerm = new ErlString(string);
            }
            else if (**fmt == '\'') {     /* quoted atom ? */
                char *qatom=pquotedatom(fmt, wbuf);
                returnTerm = new ErlAtom(qatom);
            }
        }
        break;

    }

    return returnTerm;

} /* eformat */

}
}

ErlTerm * epi::type::ErlTerm::format( const std::string &formatStr, ... )
        throw (EpiParseError)  {

    ErlTerm *res=0;
    va_list ap;

    // The std::string stores spaces as 0, so sustitute ALL values = 0
    // by spaces (20).
    int length = formatStr.length();
    char *fmt = new char[length+1];
    memcpy(fmt, formatStr.c_str(), length);
    fmt[formatStr.length()] = 0;
    for (int i=0; i<length; i++) {
        if (fmt[i] == 0)
            fmt[i] = ' ';
    }

    char *parse_fmt = (char *) fmt;

    va_start(ap, formatStr);
    res = epi::parse::eformat(&parse_fmt, &ap);
    va_end(ap);

    delete [] fmt;

    if (res == 0) {
        throw EpiParseError("Parsing error.", parse_fmt - fmt);
    }

    return res;
}

