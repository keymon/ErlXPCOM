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

#ifndef _DEBUG_HPP
#define _DEBUG_HPP

///////////////////////////////////////////////////////////////////////////
// LibCWD configuration
#ifdef CWDEBUG
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <libcwd/sys.h>

// Custom debug channels
#define DEBUGCHANNELS epi::debug::channels
#include <libcwd/debug.h>

namespace epi {
namespace debug {
namespace channels {
namespace dc {
    using namespace libcwd::channels::dc;

    // Channel for erlang basic type messages
    extern libcwd::channel_ct erlang;
    extern libcwd::channel_ct buffer;
    extern libcwd::channel_ct erlang_warning;
    extern libcwd::channel_ct erlang_memory;
    // Channel for comunication
    extern libcwd::channel_ct connect;

    // Channel for core
    extern libcwd::channel_ct epi_core;

}
}
}
}

#include <libcwd/debug.h>

/**
 * Scope protected continued
 */
class ScopedCwdContinued {
public:
    inline ScopedCwdContinued(std::string final_msg): 
    		do_finish(true), msg(final_msg) {}

    inline void finish() {
        do_finish = false;

    }

    inline ~ScopedCwdContinued() {
        if (do_finish) {
            Dout(dc::finish, msg);
        }
    }
private:
    bool do_finish;
    std::string msg;
};

// continued_cf does not work well
/*
#define Dout_continue(cntrl, name, final, data) \
    ScopedCwdContinued name(final);\
    LibcwDout(LIBCWD_DEBUGCHANNELS, ::libcwd::libcw_do, cntrl|continued_cf, data)

#define Dout_continued(data) \
	LibcwDout(LIBCWD_DEBUGCHANNELS, ::libcwd::libcw_do, dc::continued, data)

#define Dout_finish(name, data) \
    name.finish();\
	LibcwDout(LIBCWD_DEBUGCHANNELS, ::libcwd::libcw_do, dc::finish, data)
*/
#define Dout_continue(cntrl, name, final, data) \
    LibcwDout(LIBCWD_DEBUGCHANNELS, ::libcwd::libcw_do, cntrl, data)

#define Dout_finish(name, data) \
	LibcwDout(LIBCWD_DEBUGCHANNELS, ::libcwd::libcw_do, dc::notice, data)

#define Dout_continued(data) \
	LibcwDout(LIBCWD_DEBUGCHANNELS, ::libcwd::libcw_do, dc::notice, data)


#else // CWDEBUG
// Needed to define empty debug macros
#include "nodebug.h"
#endif // CWDEBUG

#endif // _DEBUG_HPP
