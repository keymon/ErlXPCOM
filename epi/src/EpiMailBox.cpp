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


#include "EpiMailBox.hpp"
#include "EpiBuffer.hpp"
#include "PatternMatchingGuard.hpp"
#include "ErlTypes.hpp"

using namespace epi::node;
using namespace epi::type;

bool MailBoxGuard::check(void* ptr) {
    EpiMessage *msg = (EpiMessage *) ptr;
    if (msg == 0) {
        return false;
    }
    ErlangMessage *emsg;
    switch(msg->messageType()) {
        case ERL_MSG_ERROR:
            // We will return true for error messages, so
            // the mailbox receive method can get then and launch
            // an exception.
            return true;
            break;
        case ERL_MSG_SEND:
        case ERL_MSG_REG_SEND:
            // We get the Erlang message and delegate it to match method
            emsg = (ErlangMessage *) msg;
            try {
                return this->match(emsg);
            } catch (EpiException &e) {
                // FIXME: What do with this exception???
                Dout(dc::connect, "["<<this<<"]"<<
                        "Exception executing MailBoxGuard: "
                                << e.getMessage());
                return false;
            }
            break;
        default:
            return false;
            break;
    }

}



MailBox::MailBox(ErlPid *self): mSelf(self) {
    Dout(dc::connect, "["<< this << "]" << "MailBox::MailBox(" << self->toString() << ")");
}

MailBox::~MailBox( )
{
    Dout(dc::connect, "MailBox::~MailBox");

    // Delete all pending messages
    mQueue.flush();

    // Notify destruction to all observers
    notify(EVENT_DESTROY);

}

OutputBuffer* MailBox::newOutputBuffer() {
    return mSender->newOutputBuffer();
}

void MailBox::deliver( void *origin, epi::node::EpiMessage* msg ) {
    Dout(dc::connect, "["<<this<<"]"<< "MailBox::deliver(msg)");
    switch(msg->messageType()) {

    case ERL_MSG_ERLANG:
    case ERL_MSG_ERROR:
        // What about put this at the head of queue???
    case ERL_MSG_SEND:
    case ERL_MSG_REG_SEND:
        mQueue.put(msg);
        break;
    case ERL_MSG_CONTROL:
    case ERL_MSG_UNLINK:
    case ERL_MSG_LINK:
    case ERL_MSG_EXIT:
        // TODO: Add code for control messages
        break;

    }
}

ErlTerm* MailBox::receive( )
        throw (EpiDecodeException, EpiConnectionException)
{
    Dout_continue(dc::connect, _continue, " failed.",
    		"["<< this << "]" << "MailBox::receive()");
    std::auto_ptr<EpiMessage> msg(mQueue.get());
    if (msg->instanceOf(ERL_MSG_ERROR)) {
        Dout_finish(_continue, " Error");
        throw *(((ErrorMessage *) msg.get())->getException());
    } else if (msg->instanceOf(ERL_MSG_ERLANG)) {
        Dout_finish(_continue, " Data");
        // Use the drop method to return a 0 refereced counted term
        ErlTermPtr<> received_term = ((ErlangMessage *) msg.get())->getMsg();
        msg.reset();
		return received_term.drop();
    } else {
        Dout_finish(_continue, " Unknown");
        throw EpiConnectionException("Unknown messageType");
    }
}

ErlTerm* MailBox::receive( long timeout )
        throw (EpiDecodeException, EpiConnectionException)
{
    Dout_continue(dc::connect, _continue, " failed.",  "["<< this << "]" << "MailBox::receive(" << timeout << ")");
    std::auto_ptr<EpiMessage> msg;
    msg.reset(mQueue.get(timeout));
	if (msg.get() == 0) {
        return 0;
	}
    if (msg->instanceOf(ERL_MSG_ERROR)) {
        Dout_finish(_continue, " Error");
        throw *(((ErrorMessage *) msg.get())->getException());
    } else if (msg->instanceOf(ERL_MSG_ERLANG)) {
        Dout_finish(_continue, " Data");
        // Use the drop method to return a 0 refereced counted term
        ErlTermPtr<> received_term = ((ErlangMessage *) msg.get())->getMsg();
        msg.reset();
		return received_term.drop();
    } else {
        Dout_finish(_continue, " Unknown");
        throw EpiConnectionException("Unknown messageType");
    }
}

InputBuffer* MailBox::receiveBuf( )
        throw (EpiConnectionException)
{
    Dout_continue(dc::connect, _continue, " failed.",  "["<< this << "]" << "MailBox::receiveBuf()");
    std::auto_ptr<EpiMessage> msg(mQueue.get());

    if (msg->instanceOf(ERL_MSG_ERROR)) {
        Dout_finish(_continue, " Error");
        throw *(((ErrorMessage *) msg.get())->getException());
    } else if (msg->instanceOf(ERL_MSG_ERLANG)) {
        Dout_finish(_continue, " Data");
        return ((ErlangMessage *) msg.get())->releaseBuffer();
    } else {
        Dout_finish(_continue, " Unknown");
        throw EpiConnectionException("Unknown messageType");
    }
}

InputBuffer* MailBox::receiveBuf( long timeout )
        throw (EpiConnectionException)
{
    Dout_continue(dc::connect, _continue, " failed.",  "["<< this << "]" << "MailBox::receiveBuf(" << timeout << ")");
    std::auto_ptr<EpiMessage> msg;
    msg.reset(mQueue.get(timeout));
	if (msg.get() == 0) {
        return 0;
	}
    if (msg->instanceOf(ERL_MSG_ERROR)) {
        Dout_finish(_continue, " Error");
        throw *(((ErrorMessage *) msg.get())->getException());
    } else if (msg->instanceOf(ERL_MSG_ERLANG)) {
        Dout_finish(_continue, " Data");
        return ((ErlangMessage *) msg.get())->releaseBuffer();
    } else {
        Dout_finish(_continue, " Unknown");
        throw EpiConnectionException("Unknown messageType");
    }
}

ErlangMessage* MailBox::receiveMsg( )
        throw (EpiConnectionException)
{
    Dout_continue(dc::connect, _continue, " failed.",  "["<< this << "]" << "MailBox::receiveMsg()");

    std::auto_ptr<EpiMessage> msg(mQueue.get());
    if (msg->instanceOf(ERL_MSG_ERROR)) {
        EpiConnectionException exception(*(((ErrorMessage *) msg.get())->getException()));
        Dout_finish(_continue, exception.getMessage());
        throw exception;
        //throw *(((ErrorMessage *) msg.get())->getException());
    } else if (msg->instanceOf(ERL_MSG_ERLANG)) {
        Dout_finish(_continue, " Data");
        return (ErlangMessage *) msg.release();
    } else {
        Dout_finish(_continue, " Unknown");
        throw EpiConnectionException("Unknown messageType");
    }
}

ErlangMessage* MailBox::receiveMsg( long timeout )
        throw (EpiConnectionException)
{
    Dout_continue(dc::connect, _continue, " failed.",  "["<< this << "]" << "MailBox::receiveMsg(" << timeout << ")");
    std::auto_ptr<EpiMessage> msg;
    msg.reset(mQueue.get(timeout));
	if (msg.get() == 0) {
        return 0;
	}

    if (msg->instanceOf(ERL_MSG_ERROR)) {
        Dout_finish(_continue, " Error");
        throw *(((ErrorMessage *) msg.get())->getException());
    } else if (msg->instanceOf(ERL_MSG_ERLANG)) {
        Dout_finish(_continue, " Data");
        return (ErlangMessage *) msg.release();
    } else {
        Dout_finish(_continue, " Unknown");
        throw EpiConnectionException("Unknown messageType");
    }
}

ErlTerm* MailBox::receive( ErlTerm *pattern, VariableBinding *binding )
        throw (EpiConnectionException)
{
    Dout_continue(dc::connect, _continue, " failed.",  "["<< this << "]" << "MailBox::receive(" << pattern->toString() << ")");
    // Create a PatternMatchingGuard
    PatternMatchingGuard guard(pattern, binding);
    std::auto_ptr<EpiMessage> msg(mQueue.get(&guard));
    if (msg->instanceOf(ERL_MSG_ERROR)) {
        Dout_finish(_continue, " Error");
        throw *(((ErrorMessage *) msg.get())->getException());
    } else if (msg->instanceOf(ERL_MSG_ERLANG)) {
        Dout_finish(_continue, " Data");
        // Use the drop method to return a 0 refereced counted term
        ErlTermPtr<> received_term(((ErlangMessage *) msg.get())->getMsg());
        msg.reset();
        return received_term.drop();
    } else {
        Dout_finish(_continue, " Unknown");
        throw EpiConnectionException("Unknown messageType");
    }

}


ErlTerm* MailBox::receive( ErlTerm *pattern, long timeout, VariableBinding *binding )
        throw (EpiConnectionException)
{
    Dout_continue(dc::connect, _continue, " failed.",
    		 "["<< this << "]" << "MailBox::receive(" << 
    		 pattern->toString() << ")");
    // Create a PatternMatchingGuard
    PatternMatchingGuard guard(pattern, binding);
    std::auto_ptr<EpiMessage> msg;
    msg.reset(mQueue.get(&guard, timeout));
	if (msg.get() == 0) {
        return 0;
	}
    if (msg->instanceOf(ERL_MSG_ERROR)) {
        Dout_finish(_continue, " Error");
        throw *(((ErrorMessage *) msg.get())->getException());
    } else if (msg->instanceOf(ERL_MSG_ERLANG)) {
        Dout_finish(_continue, " Data");
        // Use the drop method to return a 0 refereced counted term
        ErlTermPtr<> received_term = ((ErlangMessage *) msg.get())->getMsg();
        msg.reset();
        return received_term.drop();
    } else {
        Dout_finish(_continue, " Unknown");
        throw EpiConnectionException("Unknown messageType");
    }

}

ErlangMessage* MailBox::receive( MailBoxGuard* guard )
        throw (EpiConnectionException)
{
    Dout_continue(dc::connect, _continue, " failed.",  "["<< this << "]" << "MailBox::receive(guard=" << guard << ")");

    std::auto_ptr<EpiMessage> msg(mQueue.get(guard));
    if (msg->instanceOf(ERL_MSG_ERROR)) {
        Dout_finish(_continue, " Error");
        throw *(((ErrorMessage *) msg.get())->getException());
    } else if (msg->instanceOf(ERL_MSG_ERLANG)) {
        Dout_finish(_continue, " Data");
        return (ErlangMessage *) msg.release();
    } else {
        Dout_finish(_continue, " Unknown");
        throw EpiConnectionException("Unknown messageType");
    }
}

ErlangMessage* MailBox::receive( MailBoxGuard* guard, long timeout )
            throw (EpiConnectionException)
{
    Dout_continue(dc::connect, _continue, " failed.",  "["<< this << "]" <<
            "MailBox::receive(guard=" << guard << ", timeout =" << timeout<< ")");
    std::auto_ptr<EpiMessage> msg;
    msg.reset(mQueue.get(guard, timeout));
	if (msg.get() == 0) {
        return 0;
	}
    if (msg->instanceOf(ERL_MSG_ERROR)) {
        Dout_finish(_continue, " Error");
        throw *(((ErrorMessage *) msg.get())->getException());
    } else if (msg->instanceOf(ERL_MSG_ERLANG)) {
        Dout_finish(_continue, " Data");
        return (ErlangMessage *) msg.release();
    } else {
        Dout_finish(_continue, " Unknown");
        throw EpiConnectionException("Unknown messageType");
    }
}


ErlTerm* MailBox::receiveRPC( )
        throw (EpiConnectionException, EpiBadRPC)
{
    Dout_continue(dc::connect, _continue, " failed.",  "["<< this << "]" <<
            "MailBox::receiveRPC()");

    ErlTermPtr<ErlTerm> rex_pattern(
			new ErlTuple(new ErlAtom("rex"), new ErlVariable()));
    ErlTermPtr<ErlTuple> rex((ErlTuple*) receive(rex_pattern.get()));

    Dout_continued("Received " << rex->toString());

	// check if it is a bad rpc 
    ErlTermPtr<ErlTerm> badrpc_pattern(
            new ErlTuple(new ErlAtom("rex"), 
			             new ErlTuple(new ErlAtom("badrpc"), 
									  new ErlVariable("Reason"))));
									  
	VariableBinding binding;
	if (rex->match(badrpc_pattern.get(), &binding)) {		
		ErlTerm *error = binding.search("Reason");
		Dout_finish(_continue, "BadRPC");
		throw EpiBadRPC(error);
	}

    // Whe have to extract the contained term
    ErlTermPtr<ErlTerm> response(rex->elementAt(1));
    // reset the tuple
    rex.reset();

	Dout_finish(_continue, "Returning response " << response->toString());
    // Drop the response
    return response.drop();
}

ErlTerm* MailBox::receiveRPC( long timeout )
        throw (EpiConnectionException, EpiBadRPC)
{
    ErlTermPtr<ErlTerm> rex_pattern(
            new ErlTuple(new ErlAtom("rex"), new ErlVariable()));
    ErlTermPtr<ErlTuple> rex((ErlTuple*) receive(rex_pattern.get(), timeout));

    if (rex.get() == 0) {
        return 0;
    }

	// check if it is a bad rpc 
    ErlTermPtr<ErlTerm> badrpc_pattern(
            new ErlTuple(new ErlAtom("rex"), 
			             new ErlTuple(new ErlAtom("badrpc"), 
									  new ErlVariable("Reason"))));
									  
	VariableBinding binding;
	if (rex->match(badrpc_pattern.get(), &binding)) {
		throw EpiBadRPC(binding.search("Reason"));
	}

    // Whe have to extract the contained term
    ErlTermPtr<ErlTerm> response(rex->elementAt(1));

    // reset the tuple
    rex.reset();

    // Drop the response
    return response.drop();
}

void MailBox::send( ErlPid* toPid, ErlTerm* term )
        throw(EpiInvalidTerm, EpiEncodeException, EpiConnectionException)
{
    Dout(dc::connect, "["<<this<<"]"<< "MailBox::send(" <<
            toPid->toString() << ", " << term->toString() << ")");

    std::auto_ptr<OutputBuffer> buffer(mSender->newOutputBuffer());
    buffer->writeTerm(term);
    Dout(dc::connect, "["<<this<<"]"<< " buffer = "<< buffer.get());
    sendBuf(toPid, buffer.get());
}

void MailBox::send( std::string toName, ErlTerm* term )
        throw(EpiInvalidTerm, EpiEncodeException, EpiConnectionException)
{
    Dout(dc::connect, "["<<this<<"]"<< "MailBox::send(" <<
            toName << ", " << term->toString() << ")");
    std::auto_ptr<OutputBuffer> buffer(mSender->newOutputBuffer());
    buffer->writeTerm(term);
    sendBuf(toName, buffer.get());
}

void MailBox::send( std::string nodename, std::string toName, ErlTerm* term )
        throw(EpiInvalidTerm, EpiEncodeException, EpiConnectionException)
{
    Dout(dc::connect, "["<<this<<"]"<< "MailBox::send(" <<
            nodename << ", " << toName << ", " << term->toString() << ")");
    std::auto_ptr<OutputBuffer> buffer(mSender->newOutputBuffer());
    buffer->writeTerm(term);
    sendBuf(nodename, toName, buffer.get());
}

void MailBox::sendBuf( ErlPid* toPid, OutputBuffer* buffer ) const
        throw ( EpiConnectionException )
{
    Dout(dc::connect, "["<<this<<"]"<< "MailBox::sendBuf(" <<
            toPid->toString() << ", buffer)");

    if (mSender) {
        mSender->sendBuf(self(), toPid, buffer);
    } else {
        throw EpiConnectionException("Sender for MailBox not specified");
    }

}

void MailBox::sendBuf( const std::string toName, OutputBuffer* buffer ) const
        throw(EpiConnectionException )
{
    Dout(dc::connect, "["<<this<<"]"<< "MailBox::sendBuf(" <<
            toName << ", buffer)");

    if (mSender) {
        mSender->sendBuf(self(), toName, buffer);
    } else {
        throw EpiConnectionException("Sender for MailBox not specified");
    }
}

void MailBox::sendBuf( const std::string nodename,
                       const std::string toName,
                       epi::node::OutputBuffer* buffer ) const
            throw(EpiConnectionException )
{
    Dout(dc::connect, "["<<this<<"]"<< "MailBox::sendBuf(" <<
            nodename << ", " << toName << ", buffer)");

    if (mSender) {
        mSender->sendBuf(self(), nodename, toName, buffer);
    } else {
        throw EpiConnectionException("Sender for MailBox not specified");
    }
}


void MailBox::setSender( EpiSender* sender ) {
    mSender = sender;
}


void MailBox::sendRPC( const std::string nodename,
                       const std::string mod,
                       const std::string fun,
                       ErlList* args )
        throw ( EpiBadArgument, EpiInvalidTerm,
                EpiEncodeException, EpiConnectionException )
{
    ErlTerm* inner_terms[] = {
        new ErlAtom("call"),
        new ErlAtom(mod.c_str()),
        new ErlAtom(fun.c_str()),
        args,
        new ErlAtom("user")
    };
    ErlTermPtr<ErlTerm> execTuple =
            new ErlTuple(self(), new ErlTuple(inner_terms, 5));
    std::auto_ptr<OutputBuffer> buffer(mSender->newOutputBuffer());
    buffer->writeTerm(execTuple.get());
    sendBuf(nodename, "rex", buffer.get());
}

ErlTerm* MailBox::RPC(const std::string nodename,
					  const std::string mod,
                      const std::string fun,
                      ErlList* args)
        throw ( EpiBadArgument, EpiInvalidTerm,
                EpiEncodeException, EpiConnectionException,
				EpiBadRPC )
{
		this->sendRPC(nodename, mod, fun, args);
		return this->receiveRPC();
}

ErlTerm* MailBox::RPC(const std::string nodename,
					  const std::string mod,
                      const std::string fun,
                      ErlList* args, 
					  long timeout)
        throw ( EpiBadArgument, EpiInvalidTerm,
                EpiEncodeException, EpiConnectionException )
{
		this->sendRPC(nodename, mod, fun, args);
		return this->receiveRPC(timeout);
}


void MailBox::exit(ErlAtom* reason)
{
    // TODO: Not Implemented
}

void MailBox::link(ErlPid* pid)
{
    // TODO: Not Implemented
}

void MailBox::unlink(ErlPid* pid)
{
    // TODO: Not Implemented
}

