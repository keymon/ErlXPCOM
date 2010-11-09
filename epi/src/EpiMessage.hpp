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

#ifndef _EPIMESSAGE_HPP
#define _EPIMESSAGE_HPP

#include "ErlTypes.hpp"
#include "EpiBuffer.hpp"
#include "EpiInputBuffer.hpp"
#include "EpiOutputBuffer.hpp"

namespace epi {
namespace node {

using namespace epi::error;
using namespace epi::type;
using namespace epi::node;

enum EpiMessageType {
    ERL_MSG_ERROR,
    ERL_MSG_ERLANG,
    ERL_MSG_SEND,
    ERL_MSG_REG_SEND,
    ERL_MSG_CONTROL,
    ERL_MSG_UNLINK,
    ERL_MSG_LINK,
    ERL_MSG_EXIT
};

class Connection;


/**
 * Representation of Erlang messages
 */
class EpiMessage {
public:

    /**
     * Get the type of this message
     */
    virtual EpiMessageType messageType() = 0;

    /**
     * Check if this message corresponds to the given type
     */
    virtual bool instanceOf(EpiMessageType type) = 0;

    /** Virtual destructor  */
    virtual inline ~EpiMessage() {}
};

/**
 * Error message. contains an exception.
 */
class ErrorMessage: public EpiMessage {
public:
    inline EpiMessageType messageType() {
        return ERL_MSG_ERROR;
    }
    inline bool instanceOf(EpiMessageType type) {
        return type == ERL_MSG_ERROR;
    }
    inline ErrorMessage(EpiConnectionException *e): mException(e) {}
    inline EpiConnectionException *getException() {
        return mException;
    }

    /**
     * Get the exception and set the internal pointer
     * to 0
     */
    inline EpiConnectionException *releaseException() {
        EpiConnectionException * tmp = mException;
        mException = 0;
        return tmp;
    }

    /**
     * The exception WILL BE DELETED on message destruction
     */
    virtual ~ErrorMessage() {
        delete mException;
    }
private:
    EpiConnectionException *mException;
};

/**
 * An Erlang message.
 */
class ErlangMessage: public EpiMessage {
public:
    inline InputBuffer* getBuffer() {
        return mBuffer;
    }

    /**
     * Get the buffer and set the internal pointer to 0
     */
    inline InputBuffer* releaseBuffer() {
        InputBuffer* tmp = mBuffer;
        mBuffer = 0;
        return tmp;
    }

    /**
     * Check if there is an decoded term in this message.
     * This method can be used to fast check if there is a valid
     * term to check in this message.
     */
    inline bool hasMsg() {
        return mPayLoad.get() != 0;
    }

    /**
     * Decode and get containing term
     */
    inline ErlTerm *getMsg()
            throw (EpiDecodeException)
    {
        if (mPayLoad.get() == 0)  {
            try {
                mPayLoad.reset(mBuffer->readTerm());
            } catch (EpiDecodeException &e) {
                mBuffer->resetIndex();
                throw e;
            }
            mBuffer->resetIndex();
        }
        return mPayLoad.get();
    }

    /**
     * The Buffer WILL BE DELETED on message destruction
     */
    virtual ~ErlangMessage() {
        delete mBuffer;
    }
protected:
    /**
     * Buffer will be decoded in message construction, and the
     * buffer index will be reset after decoding.
     * If there is an error, it will ignore it, but the term
     * will not be decoded. So, the error can be received later.
     */
    inline ErlangMessage(InputBuffer *buffer): mBuffer(buffer) {
        try {
            this->getMsg();
        } catch (EpiDecodeException &e) {
        }
    }

private:
    InputBuffer *mBuffer;
    ErlTermPtr<ErlTerm> mPayLoad;
};

/**
 * Send message
 */
class SendMessage: public ErlangMessage {
public:
    inline SendMessage(ErlPid* recipient, InputBuffer *buffer):
        ErlangMessage(buffer), mRecipient(recipient) {}
    inline ErlPid *getRecipientPid() {
        return mRecipient.get();
    }
    inline EpiMessageType messageType() {
        return ERL_MSG_SEND;
    }
    inline bool instanceOf(EpiMessageType type) {
        return type == ERL_MSG_SEND || type == ERL_MSG_ERLANG;
    }

    /** Virtual destructor  */
    virtual inline ~SendMessage() {}

private:
    ErlTermPtr<ErlPid> mRecipient;
};

/**
 * Send message to a registered name
 */
class RegSendMessage: public ErlangMessage {
public:
    inline RegSendMessage(ErlPid *sender,
                          std::string recipient,
                          InputBuffer *buffer):
        ErlangMessage(buffer), mSender(sender), mRecipient(recipient) {}
    inline ErlPid *getSenderPid() {
        return mSender.get();
    }
    inline std::string getRecipientName() {
        return mRecipient;
    }
    inline EpiMessageType messageType() {
        return ERL_MSG_REG_SEND;
    }
    inline bool instanceOf(EpiMessageType type) {
        return type == ERL_MSG_REG_SEND || type == ERL_MSG_ERLANG;
    }
    /** Virtual destructor  */
    virtual inline ~RegSendMessage() {}
private:
    ErlTermPtr<ErlPid> mSender;
    std::string mRecipient;
};

/**
 * An control message.
 */
class ControlMessage:public EpiMessage {
public:
    inline ErlPid* getRecipientPid() {
        return mRecipient.get();
    }
    inline ErlPid* getSenderPid() {
        return mSender.get();
    }
    virtual inline ~ControlMessage() {}
protected:
    inline ControlMessage(ErlPid* sender, ErlPid* recipient):
        mSender(sender), mRecipient(recipient) {}

private:
    ErlTermPtr<ErlPid> mSender;
    ErlTermPtr<ErlPid> mRecipient;

};

/**
 * Link message
 */
class LinkMessage: public ControlMessage {
public:
    inline LinkMessage(ErlPid* sender, ErlPid* recipient):
        ControlMessage(sender, recipient) {}

    inline EpiMessageType messageType() {
        return ERL_MSG_LINK;
    }
    inline bool instanceOf(EpiMessageType type) {
        return type == ERL_MSG_LINK || type == ERL_MSG_CONTROL;
    }
    virtual inline ~LinkMessage() {}
};

/**
 * UnLink message
 */
class UnLinkMessage: public ControlMessage {
public:
    inline UnLinkMessage(ErlPid* sender, ErlPid* recipient):
        ControlMessage(sender, recipient) {}

    inline EpiMessageType messageType() {
        return ERL_MSG_UNLINK;
    }
    inline bool instanceOf(EpiMessageType type) {
        return type == ERL_MSG_UNLINK || type == ERL_MSG_CONTROL;
    }
    virtual inline ~UnLinkMessage() {}
};

/**
 * Exit message
 */
class ExitMessage: public ControlMessage {
public:
    inline ExitMessage(ErlPid* sender, ErlPid* recipient, ErlAtom* reason):
        ControlMessage(sender, recipient), mReason(reason) {}

    inline EpiMessageType messageType() {
        return ERL_MSG_EXIT;
    }
    inline bool instanceOf(EpiMessageType type) {
        return type == ERL_MSG_EXIT || type == ERL_MSG_CONTROL;
    }
    virtual inline ~ExitMessage() {}
private:
    ErlTermPtr<ErlAtom> mReason;
};

} // namespace node
} // namespace epi
#endif
