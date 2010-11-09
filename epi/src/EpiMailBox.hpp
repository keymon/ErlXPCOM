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

#ifndef __MAILBOX_HPP
#define __MAILBOX_HPP

#include "ErlTypes.hpp"

#include "GenericQueue.hpp"
#include "EpiReceiver.hpp"
#include "EpiSender.hpp"
#include "EpiObserver.hpp"
#include "EpiMessage.hpp"

namespace epi {
namespace node {

using namespace epi::type;

/**
 * This class allows you to explore the MailBox Queue.
 */
class MailBoxGuard: public QueueGuard {
public:
    /**
     * Callback method from GenericQueue. This will delegate
     * on match method.
     */
    bool check(void* elem);

    /**
     * You must implement this method to explore the mailbox
     * queue.
     * @param msg a pointer to a ErlangMessage for
     * each message in mailbox.
     * @return true to match the message and the message will
     * be returned to the user. Return false otherwise
     * @throw EpiException is there is an error.
     * FIXME: Actually the exception is not propagated!!! :-/
     * Reimplement the GenericQueue.
     *
     */
    virtual bool match(ErlangMessage *msg) throw (EpiException) = 0;

};

/**
 * Provides a simple mechanism for exchanging messages with Erlang
 * processes or other instances of this class.
 *
 * MailBox is the way to send an receive messages. Outgoing messages
 * will be forwarded to a EpiSender class associated to this mailbox.
 * The sender will be ussually Connection or a AutoNode.
 *
 * MailBox is a EpiReceiver, and will receive and queue messages
 * delivered with the deliver() method. MailBox will accept and
 * queue all messages delivered.
 *
 * You can get the message in order of arrival, or use pattern matching
 * or guards to explore message queue.
 *
 * Each mailbox is associated with a unique {@link OtpErlangPid
 * pid} that contains information necessary for delivery of messages.
 *
 * Messages to remote nodes are externalized for transmission, and
 * as a result the recipient receives a <b>copy</b> of the original
 * object.
 *
 * TODO: Additionally, mailboxes can be linked in much the same way as
 * Erlang processes. If a link is active when a mailbox is {@link
 * #close closed}, any linked Erlang processes or OtpMboxes will be
 * sent an exit signal. As well, exit signals will be (eventually)
 * sent if a mailbox goes out of scope and its {@link #finalize
 * finalize()} method called. However due to the nature of
 * finalization (i.e. Java makes no guarantees about when {@link
 * #finalize finalize()} will be called) it is recommended that you
 * always explicitly close mailboxes if you are using links instead of
 * relying on finalization to notify other parties in a timely manner.
 *
 * TODO: When retrieving messages from a mailbox that has received an exit
 * signal, an {@link OtpErlangExit OtpErlangExit} exception will be
 * raised. Note that the exception is queued in the mailbox along with
 * other messages, and will not be raised until it reaches the head of
 * the queue and is about to be retrieved. </p>
 *
 */
class MailBox: public EpiReceiver, public EpiObservable {

public:

    MailBox(ErlPid *self);

    virtual ~MailBox( );


    inline ErlPid *self() const {
        return mSelf.get();
    }

    /**
     * Create a new OutputBuffer to be used with this mailbox
     */
    OutputBuffer* newOutputBuffer();

    /**
     * Deliver a message to this mailbox
     */
	void deliver( void *origin, EpiMessage* msg );

    /**
     * Get a message from this mailbox.
     * Block until a message arrives for this mailbox.
     * @return an pointer to the ErlTerm representing
     * the body of the next message waiting in this mailbox.
     * @exception EpiDecodeException if the message can not be decoded.
     * @exception EpiConnectionException if there was an connection error
     **/
    ErlTerm* receive( )
            throw (EpiDecodeException, EpiConnectionException);

    /**
     * Get a message from this mailbox.
     * Block until  a message arrives for this mailbox no more
     * than timeout ms.
     * @param timeout the time, in milliseconds, to wait for a message
     * before returning 0.
     * @return an pointer to the ErlTerm representing
     * the body of the next message waiting in this mailbox, or
     * 0 if timeout is reached.
     * @exception EpiDecodeException if the message can not be * decoded.
     * @exception EpiConnectionException if there was an connection error
     **/
    ErlTerm* receive( long timeout )
            throw (EpiDecodeException, EpiConnectionException);

    /**
     * Block until a message arrives for this mailbox.
     * @return an InputBuffer representing the still-encoded body of the
     * next message waiting in this mailbox.
     * @exception EpiConnectionException if there was an connection error
     **/
    InputBuffer* receiveBuf( )
            throw (EpiConnectionException);

    /**
     * Block until a message arrives for this mailbox.
     * @param timeout the time, in milliseconds, to wait for a message
     * before returning 0.
     * @return an InputBuffer representing the still-encoded body of the
     * next message waiting in this mailbox, or 0 on timeout.
     * @exception EpiConnectionException if there was an connection error
     **/
    InputBuffer* receiveBuf( long timeout )
            throw (EpiConnectionException);

    /**
     * Block until a message arrives for this mailbox.
     * @return a pointer to ErlangMessage containing the header
     * information as well as the body of the next message waiting in
     * this mailbox.
     * @exception EpiConnectionException if there was an connection error
     **/
	ErlangMessage* receiveMsg( )
            throw (EpiConnectionException);

    /**
     * Block until a message arrives for this mailbox.
     * @param timeout the time, in milliseconds, to wait for a message
     * before returning 0.
     * @return a pointer to ErlangMessage containing the header
     * information as well as the body of the next message waiting in
     * this mailbox, or 0 on timeout.
     * @exception EpiConnectionException if there was an connection error
     **/
    ErlangMessage* receiveMsg( long timeout )
            throw (EpiConnectionException);

    /**
     * Get a message from mailbox that matches the given pattern.
     * It will block until an apropiate message arrives.
     * @param pattern ErlTerm with pattern to check
     * @param binding VariableBinding to use. It can be 0. Default = 0
     * @return an pointer to the ErlTerm representing
     * the body of the next message waiting in this mailbox.
     * @exception EpiConnectionException if there was an connection error
     */
    ErlTerm* receive( ErlTerm *pattern, VariableBinding *binding = 0  )
            throw (EpiConnectionException);

    /**
     * Get a message from mailbox that matches the given pattern.
     * @param pattern ErlTerm with pattern to check
     * @param timeout the time, in milliseconds, to wait for a message
     * before returning 0.
     * @param binding VariableBinding to use. It can be 0. Default = 0
     * @return an pointer to the ErlTerm representing
     * the body of the next message waiting in this mailbox.
     * @exception EpiConnectionException if there was an connection error
     */
    ErlTerm* receive( ErlTerm *pattern, long timeout, VariableBinding *binding = 0 )
            throw (EpiConnectionException);

    /**
     * Get a message from mailbox that satisfaces given guard (See MailBoxGuard).
     * It will block until an apropiate message arrives.
     * @param guard Guard to check. Owership is not transfered
     * @return an pointer to the ErlangMessage that satisfaces the guard.
     * @exception EpiConnectionException if there was an connection error
     */
    ErlangMessage* receive( MailBoxGuard* guard )
            throw (EpiConnectionException);

    /**
     * Get a message from mailbox that satisfaces given guard (See MailBoxGuard).
     * It will block until an apropiate message arrives.
     * @param guard Guard to check. Owership is not transfered
     * @param timeout the time, in milliseconds, to wait for a message
     * before returning 0.
     * @return an pointer to the ErlangMessage that satisfaces the guard.
     * @exception EpiConnectionException if there was an connection error
     */
    ErlangMessage* receive( MailBoxGuard* guard, long timeout )
            throw (EpiConnectionException);

    /**
     * Block until response for a RPC call arrives.
     * @return a pointer to ErlTerm containing the response
     * @exception EpiConnectionException if there was an connection error
	 * @throw EpiBadRPC if the corresponding RPC was incorrect
     **/
    ErlTerm* receiveRPC( )
            throw (EpiConnectionException, EpiBadRPC);

    /**
     * Block until response for a RPC call arrives.
     * @param timeout the time, in milliseconds, to wait for a message
     * before returning 0.
     * @return a pointer to ErlTerm containing the response, or 0 on timeout
	 * @throw EpiConnectionException if there is any problem with the connection
	 * @throw EpiBadRPC if the corresponding RPC was incorrect
     **/
    ErlTerm* receiveRPC( long timeout )
            throw (EpiConnectionException, EpiBadRPC);

    void send( epi::type::ErlPid* toPid, ErlTerm* term )
            throw(EpiInvalidTerm, EpiEncodeException, EpiConnectionException);

    void send( std::string toName, ErlTerm* term )
            throw(EpiInvalidTerm, EpiEncodeException, EpiConnectionException);

    void send( std::string nodename, std::string toName, ErlTerm* term )
            throw(EpiInvalidTerm, EpiEncodeException, EpiConnectionException);

	/**
     * Send data to a pid
	 * @param aPid remote process pid
	 * @param buffer Buffer to send
     * @throws EpiConnectionException if send fails
	 */
    void sendBuf( epi::type::ErlPid* toPid,
                  epi::node::OutputBuffer* buffer ) const
            throw(EpiConnectionException);
    /**
     * Send data to a registered server
     * @param aServerName remote server name
     * @param buffer Buffer to send
     * @throws EpiConnectionException if send fails
     */
    void sendBuf( const std::string toName, epi::node::OutputBuffer* buffer ) const
            throw(EpiConnectionException);

    /**
     * Send data to a registered server on a remote server
     * @param aServerName remote server name
     * @param buffer Buffer to send
     * @throws EpiConnectionException if send fails
     */
    void sendBuf( const std::string nodename,
                  const std::string toName,
                  epi::node::OutputBuffer* buffer ) const
            throw(EpiConnectionException);

    /**
	 * Send an RPC request to a remote Erlang node.
     * @param node remote node where execute the funcion.
	 * @param mod the name of the Erlang module containing the
	 * function to be called.
	 * @param fun the name of the function to call.
	 * @param args a list of Erlang terms, to be used as arguments
	 * to the function.
     * @throws EpiBadArgument if function, module or nodename are too big
     * @throws EpiInvalidTerm if any of the args is invalid
	 * @throws EpiEncodeException if encoding fails
     * @throws EpiConnectionException if send fails
	 */
    void sendRPC( const std::string nodename,
                  const std::string mod,
                  const std::string fun,
                  ErlList* args )
            throw ( EpiBadArgument, EpiInvalidTerm,
                    EpiEncodeException, EpiConnectionException);

    /**
	 * Send an RPC request to a remote Erlang node and
     * block until response for a RPC call arrives.
     * @param node remote node where execute the funcion.
	 * @param mod the name of the Erlang module containing the
	 * function to be called.
	 * @param fun the name of the function to call.
	 * @param args a list of Erlang terms, to be used as arguments
	 * to the function.
     * @return a pointer to ErlTerm containing the response
     **/
	ErlTerm* RPC(const std::string nodename,
				 const std::string mod,
                 const std::string fun,
                 ErlList* args)
        throw ( EpiBadArgument, EpiInvalidTerm,
                EpiEncodeException, EpiConnectionException,
				EpiBadRPC );

    /**
	 * Send an RPC request to a remote Erlang node and
     * block until response for a RPC call arrives.
     * @param node remote node where execute the funcion.
	 * @param mod the name of the Erlang module containing the
	 * function to be called.
	 * @param fun the name of the function to call.
	 * @param args a list of Erlang terms, to be used as arguments
	 * to the function.
     * @param timeout the time, in milliseconds, to wait for a message
     * before returning 0.
     * @return a pointer to ErlTerm containing the response, or 0 on timeout
     **/
	ErlTerm* RPC(const std::string nodename,
				 const std::string mod,
                 const std::string fun,
                 ErlList* args, 
				 long timeout)
        throw ( EpiBadArgument, EpiInvalidTerm,
                EpiEncodeException, EpiConnectionException );


    /**
     * Send exit message to all linked nodes
     */
    void exit(ErlAtom* reason);

    /**
     * Link to the given pid
     */
    void link(ErlPid* pid);

    /**
     * UnLink the given pid
     */
    void unlink(ErlPid* pid);

    /**
     * Link this mailbox to the given pid. The given pid will receive
     * exit messages.
     */
    void linkMailBox(ErlPid* pid);

    /**
     * UnLink this mailbox to the given pid. So the given pid will not
     * receive exit messages.
     */
    void unLinkMailBox(ErlPid* pid);

    /**
     * Set sender for this mailbox
     */
    void setSender( EpiSender* sender );



private:
    ErlTermPtr<ErlPid> mSelf;

	EpiSender *mSender;

    GenericQueue<EpiMessage> mQueue;
};

} // namespace node
} // namespace epi

#endif // __MAILBOX_HPP
