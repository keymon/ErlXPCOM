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

#ifndef __EPIAUTONODE_HPP
#define __EPIAUTONODE_HPP

#include <string>
#include <map>
#include <list>
#include <functional>
#include <OpenThreads/Thread>
#include <OpenThreads/Mutex>

#include "EpiException.hpp"
#include "ErlTypes.hpp"
#include "EpiLocalNode.hpp"
#include "EpiConnection.hpp"
#include "EpiMailBox.hpp"

namespace epi {
namespace node {

using namespace epi::type;
using namespace epi::error;

/**
 * Represents a local auto managed node. This class is used when you do not
 * wish to manage connections yourself - outgoing connections are
 * established as needed, and incoming connections accepted
 * automatically. The programmer have to use the mailbox API for
 * communication, while management of the underlying communication
 * mechanism is automatic and hidden from the application programmer.
 *
 * Once an instance of this class has been created, obtain one or
 * more mailboxes in order to send or receive messages. The first
 * message sent to a given node will cause a connection to be set up
 * to that node. Any messages received will be delivered to the
 * appropriate mailboxes.
 *
 * Mailboxes can be named using registerMailBox(). Messages
 * can be sent to named mailboxes and named Erlang processes without
 * knowing the {@link OtpErlangPid pid} that identifies the mailbox.
 * This is neccessary in order to set up initial communication between
 * parts of an application.
 * A MailBox can have one or more names.
 *
 * To shut down the node, call {@link #close close()}. This will
 * prevent the node from accepting additional connections and it will
 * cause all existing connections to be closed. Any unread messages in
 * existing mailboxes can still be read, however no new messages will
 * be delivered to the mailboxes.
 *
 * Note that the use of this class requires that Epmd (Erlang Port
 * Mapper Daemon) is running on each cooperating host. This class does
 * not start Epmd automatically as Erlang does, you must start it
 * manually or through some other means. See the Erlang documentation
 * for more information about this.
 **/
class AutoNode: public LocalNode, public EpiReceiver,
                public EpiSender, public OpenThreads::Thread
{
    /*
     * Necessary to allow do hashmap on ErlPidPtr :-P
     */
    struct ErlPidPtrCompare :
        std::binary_function<ErlTermPtr<ErlPid>, ErlTermPtr<ErlPid>, bool>
        {
            bool operator() (const ErlTermPtr<ErlPid> &t1,
            const ErlTermPtr<ErlPid> &t2) const
            {
                if (t1.get() == 0 && t2.get() == 0) return true;
                if (t1.get() == 0 || t2.get() == 0) return false;
				std::less<ErlPid> pidless;
				return pidless(*t1.get(), *t2.get());
            }
        };


    typedef std::map<ErlTermPtr<ErlPid>, MailBox *, ErlPidPtrCompare> mailbox_map;
    typedef std::map<std::string, MailBox *> registered_mailbox_map;
    typedef std::map<std::string, Connection *> connection_map;
    typedef std::list<Connection *> connection_list;

public:
    /**
     * Create a new node, using default cookie an any port
     * @param aNodeName node name
     * @throws EpiBadArgument if node name is too long
     * @throws EpiConnectionException if there is a network problem
     */
    AutoNode(const std::string aNodeName)
            throw (EpiBadArgument, EpiConnectionException);

    /**
     * Create a new node, using given cookie
     * @param aNodeName node name
     * @param cookie cookie to use
     * @throws EpiBadArgument if node name is too long
     * @throws EpiConnectionException if there is a network problem
     */
    AutoNode(const std::string aNodeName, const std::string aCookie)
            throw (EpiBadArgument, EpiConnectionException);

    /**
     * Create a new node, using given cookie
     * @param aNodeName node name
     * @param aCookie cookie to use
     * @param transport to use
     * @throws EpiBadArgument if node name is too long
     * @throws EpiConnectionException if there is a network problem
     */
    AutoNode(const std::string aNodeName,
             const std::string aCookie,
             ErlangTransport *transport)
            throw (EpiBadArgument, EpiConnectionException);


    ~AutoNode();

    /**
     * Create a new MailBox with a new pid associated to this node.
     * The MailBox will use this node as sender and this node
     * will deliver to this mailbox all messages with mailbox pid
     * as destination.
     *
     * MailBox pointer owership belongs to this node.
     */
    MailBox *createMailBox();

    /**
     * Deatach a mailbox from this AutoNode. The mailbox will
     * be unregistered from mailboxes lists and will not recive
     * more messages.
     * It will not be deleted and owership by this node is lost,
     * so the user MUST delete the mailbox after call this method.
     */
    void deattachMailBox(MailBox *mailbox);


    /**
     * Register an MailBox with given name. MailBox must be associated
     * to this node.
     */
    void registerMailBox(const std::string name, MailBox *mailbox);

    /**
     * Unregister a name for a mailbox
     */
    void unRegisterMailBox(const std::string name);

    /**
     * Unregister all names for this mailbox
     */
    void unRegisterMailBox(MailBox *mailbox);


    /**
     * Close this node. All connections will be closed and no
     * more connections will be created or accepted.
     */
    void close();


    /**
     * Start acceptor thread. Call this method to accept new 
     * connections automacly.
     * @throws EpiConnectionException if there are problems
     *  publishing port
     */
    void startAcceptor()
            throw (EpiConnectionException);

    /**
     * Connection acceptor thread code.
     * The node will automacly accept incomming connections.
     */
    void run();

	/**
	* Determine if another node is alive. This method has the side
	* effect of setting up a connection to the remote node (if
	* possible). Only a single outgoing message is sent; the timeout is
	* how long to wait for a response. 
	*
	* Only a single attempt is made to connect to the remote node,
	* so for example it is not possible to specify an extremely long
	* timeout and expect to be notified when the node eventually comes
	* up. If you wish to wait for a remote node to be started, the
	* following construction may be useful: 
	*
	* // ping every 2 seconds until positive response
	*  while (!me.ping(him,2000));
	*
	* @param node the name of the node to ping.
	* @param timeout the time, in milliseconds, to wait for response
	* before returning false.
	*
	* @return true if the node was alive and the correct ping response
	* was returned. false if the correct response was not returned on
	* time.
	**/
	bool ping(const std::string remoteNode, long timeout);

    /**
     * Deliver incoming message
     * This method will analize the message content, delivering it to the
     * destination mailbox.
     */
    void deliver( void *origin, EpiMessage* msg );

    /**
     * Create a new OutputBuffer to be used with this sender.
     * The output buffer used by AutoNode is a plain buffer that
     * simply stores the term.
     */
    virtual OutputBuffer* newOutputBuffer();

    /**
     * Send a buffer to a pid.
     */
    void sendBuf( epi::type::ErlPid* from,
                  epi::type::ErlPid* to,
                  epi::node::OutputBuffer* buffer )
            throw (epi::error::EpiConnectionException);

    /**
     * Send a buffer to a registered server.
     */
    void sendBuf( epi::type::ErlPid* from,
                  const std::string &to,
                  epi::node::OutputBuffer* buffer )
            throw (epi::error::EpiConnectionException);

    /**
     * Send a buffer to a registered server in the given node
     */
    void sendBuf( epi::type::ErlPid* from,
                  const std::string &node,
                  const std::string &to,
                  epi::node::OutputBuffer* buffer )
            throw (epi::error::EpiConnectionException);

    /**
     * Receive an event from an observed object. If the sender is
     * a mailbox and event is EVENT_DESTROY, mailbox will be deleted
     * from mailbox list.
     */
    void event(EpiObservable* observed, EpiEventTag event);

protected:

    /**
     * Add mailbox to the map of associated mailboxes. This node will
     * be set as sender for mailbox. This node will monitor the mailbox.
     */
    void addMailBox(MailBox *mailbox);

    /**
     * Get mailbox by pid
     */
    MailBox *getMailBox(ErlPid *pid);

    /**
     * Get mailbox by string
     */
    MailBox *getMailBox(std::string name);

    /**
     * Remove a mailbox
     */
    void removeMailBox(MailBox *mailbox);

    /**
     * Get connection by node name from internal map
     */
    Connection* getConnection(std::string name);

    /**
     * Add a connection to the map of Connections. This node will be
     * set as receiver for this connection. Connection will be started
     * and must not be running.
     * I also deletes the connections in the flush list (list of
     * connections to be deleted).
     */
    void addConnection(Connection* connection);

    /**
     * Remove and delete a connection.
     * It deletes connection only if it exists in connection map.
     * It really adds the connection to a list of connections
     * to be deleted. This allows a connection thread to
     * remove himself.
     */
    void removeConnection(Connection* connection);

    /**
     * Delete all connections in the flush list (connections to
     * be deleted). This method is not guarded, and must be
     * called from a guarded method.
     */
    void flushConnections();

    /**
     * Get the connection for given name. If no connection exists,
     * try to setup a connection, adding it to the map
     */
    Connection *attempConnection(std::string name)
            throw (EpiConnectionException);


private:

    bool mThreadExit;

    mailbox_map mMailBoxes;
    connection_map mConnections;
    registered_mailbox_map mRegMailBoxes;
    connection_list mFlushConnections;

    OpenThreads::Mutex _connectionsMutex;
    OpenThreads::Mutex _mailboxesMutex;
    OpenThreads::Mutex _regmailboxesMutex;
    OpenThreads::Mutex _socketMutex;

    /*
     * Close and delete all connections. To be used in destructor
     */
    void destroyConnections();

    /*
     * Close and delete all mailboxes. To be used in destructor
     */
    void destroyMailBoxes();

};

#if 0
/*
Look!! That's a dog!

              .--.             .---.
             /:.  '.         .' ..  '._.---.
            /:::-.  \.-"""-;` .-:::.     .::\
           /::'|  `\/  _ _  \'   `\:'   ::::|
       __.'    |   /  (o|o)  \     `'.   ':/
      /    .:. /   |   ___   |        '---'
     |    ::::'   /:  (._.) .:\
     \    .='    |:'        :::|
      `""`       \     .-.   ':/
            jgs   '---`|I|`---'
                       '-'


    :-m
*/
#endif

} // node
} // epi


#endif // __EPIAUTONODE_HPP

