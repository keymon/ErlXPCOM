#include <string>
#include <iostream>
#include <epi.hpp>

#include <string.h>

#include "SillyClass.hpp"

using namespace epi::node;
using namespace epi::error;
using namespace epi::type;

std::string LOCALNODE;
std::string REMOTENODE;
std::string COOKIE;


void test1(MailBox *mailbox) {
	SillyClass c;
	ErlTermPtr<> tuplePattern = 
		new ErlTuple(new ErlVariable("Pid"),
		             new ErlVariable("Value")); 
	int counter = 0;
	try {
		while (1) {
			VariableBinding binding;
			ErlTermPtr<> receivedTerm(mailbox->receive());
			counter ++;
			//std::cout << "Recibido: " << receivedTerm->toString() << std::endl;
			
			if (tuplePattern->match(receivedTerm.get(), &binding)) {	

				ErlPid *pid = ErlPid::cast(binding.search("Pid"));
				ErlLong *value = ErlLong::cast(binding.search("Value"));
				
				ErlTermPtr<ErlLong> 
					newValue (new ErlLong(c.increment(value->longValue())));
				
				mailbox->send(pid, newValue.get());
				
			} else {
				std::cout << "Error: incorrect format" << std::endl;
				std::cout << "Levamos " << counter << std::endl;
			}
				
		}
	} catch (EpiException &e) {
		std::cout << "Error: " << e.getMessage();
	}
}

void test1_2(MailBox *mailbox) {
	SillyClass c;
	int counter = 0;
	try {
		while (1) {
			VariableBinding binding;
			ErlTermPtr<ErlTuple> receivedTuple(
				ErlTuple::cast(mailbox->receive()));
			counter ++;
			ErlPid *pid = ErlPid::cast(receivedTuple->elementAt(0));
			ErlLong *value = ErlLong::cast(receivedTuple->elementAt(1));
				
			ErlTermPtr<ErlLong> 
				newValue (new ErlLong(c.increment(value->longValue())));
			
			mailbox->send(pid, newValue.get());
				
		}
	} catch (EpiException &e) {
		std::cout << "Error: " << e.getMessage();
	}
}

void test2(MailBox *mailbox){
	SillyClass c;
	ErlTermPtr<> tuplePattern = 
		new ErlTuple(new ErlVariable("Pid"),
		             new ErlVariable("Value1"),
					 new ErlVariable("Value2")); 
	int counter = 0;
	try {
		while (1) {
			VariableBinding binding;
			ErlTermPtr<> receivedTerm(mailbox->receive());
			counter ++;
			//std::cout << "Recibido: " << receivedTerm->toString() << std::endl;
			
			if (tuplePattern->match(receivedTerm.get(), &binding)) {	

				ErlPid *pid = ErlPid::cast(binding.search("Pid"));
				ErlString *value1 = ErlString::cast(binding.search("Value1"));
				ErlString *value2 = ErlString::cast(binding.search("Value2"));
				
				ErlTermPtr<ErlString> 
					newValue (new ErlString(
								c.concatenate(
									value1->stringValue(),
									value2->stringValue()).c_str()
								));
				
				mailbox->send(pid, newValue.get());
				
			} else {
				std::cout << "Error: incorrect format" << std::endl;
				std::cout << "Levamos " << counter << std::endl;
			}
				
		}
	} catch (EpiException &e) {
		std::cout << "Error: " << e.getMessage();
	}
}

void test2_2(MailBox *mailbox){
	SillyClass c;
	int counter = 0;
	try {
		while (1) {
			VariableBinding binding;
			ErlTermPtr<ErlTuple> receivedTuple(
				ErlTuple::cast(mailbox->receive()));
			counter ++;
			//std::cout << "Recibido: " << receivedTerm->toString() << std::endl;
			
			ErlPid *pid = ErlPid::cast(receivedTuple->elementAt(0));
			ErlString *value1 = ErlString::cast(receivedTuple->elementAt(1));
			ErlString *value2 = ErlString::cast(receivedTuple->elementAt(2));
			
			ErlTermPtr<ErlString> 
				newValue (new ErlString(
							c.concatenate(
								value1->stringValue(),
								value2->stringValue()).c_str()
						));
			
			mailbox->send(pid, newValue.get());
				
		}
	} catch (EpiException &e) {
		std::cout << "Error: " << e.getMessage();
	}
}

void test3(MailBox *mailbox){
	SillyClass c;
	ErlTermPtr<> tuplePattern1 = 
		new ErlTuple(new ErlAtom("incr"), 
					 new ErlVariable("Pid"),
		             new ErlVariable("Value")); 
	ErlTermPtr<> tuplePattern2 = 
		new ErlTuple(new ErlAtom("concat"), 
					 new ErlVariable("Pid"),
		             new ErlVariable("Value1"),
					 new ErlVariable("Value2")); 
	int counter = 0;
	try {
		while (1) {
			VariableBinding binding;
			ErlTermPtr<> receivedTerm(mailbox->receive());
			counter ++;
			//std::cout << "Recibido: " << receivedTerm->toString() << std::endl;
			
			if (tuplePattern1->match(receivedTerm.get(), &binding)) {	

				ErlPid *pid = ErlPid::cast(binding.search("Pid"));
				ErlLong *value = ErlLong::cast(binding.search("Value"));
				
				ErlTermPtr<ErlLong> 
					newValue (new ErlLong(c.increment(value->longValue())));
				
				mailbox->send(pid, newValue.get());
			
			} else if (tuplePattern2->match(receivedTerm.get(), &binding)) {	

				ErlPid *pid = ErlPid::cast(binding.search("Pid"));
				ErlString *value1 = ErlString::cast(binding.search("Value1"));
				ErlString *value2 = ErlString::cast(binding.search("Value2"));
				
				ErlTermPtr<ErlString> 
					newValue (new ErlString(
								c.concatenate(
									value1->stringValue(),
									value2->stringValue()).c_str()
								));
				
				mailbox->send(pid, newValue.get());
				
			} else {
				std::cout << "Error: incorrect format" << std::endl;
				std::cout << "Levamos " << counter << std::endl;
			}
				
		}
	} catch (EpiException &e) {
		std::cout << "Error: " << e.getMessage();
	}
}

void test3_2(MailBox *mailbox){
	SillyClass c;
	int counter = 0;
	try {
		while (1) {
			VariableBinding binding;
			ErlTermPtr<ErlTuple> receivedTuple(
				ErlTuple::cast(mailbox->receive()));
			counter ++;
			//std::cout << "Recibido: " << receivedTerm->toString() << std::endl;
			ErlAtom *atom = ErlAtom::cast(receivedTuple->elementAt(0));
			const char *atomValue = atom->atomValue();
			unsigned int arity = receivedTuple->arity();
			if (strcmp("incr", atomValue) == 0 && arity == 3) {	

				ErlPid *pid = ErlPid::cast(receivedTuple->elementAt(1));
				ErlLong *value = ErlLong::cast(receivedTuple->elementAt(2));
				
				ErlTermPtr<ErlLong> 
					newValue (new ErlLong(c.increment(value->longValue())));
				
				mailbox->send(pid, newValue.get());
			
			} else if (strcmp("concat", atomValue)==0 && arity == 4) {	

				ErlPid *pid = ErlPid::cast(receivedTuple->elementAt(1));
				ErlString *value1 = ErlString::cast(receivedTuple->elementAt(2));
				ErlString *value2 = ErlString::cast(receivedTuple->elementAt(3));
				
				ErlTermPtr<ErlString> 
					newValue (new ErlString(
								c.concatenate(
									std::string(value1->stringValue()),
									std::string(value2->stringValue())).c_str()
								));
				
				mailbox->send(pid, newValue.get());
				
			} else {
				std::cout << "Error: incorrect format" << std::endl;
				std::cout << "Levamos " << counter << std::endl;
			}
				
		}
	} catch (EpiException &e) {
		std::cout << "Error: " << e.getMessage();
	}
}


void test4(MailBox *mailbox){
	SillyClass c;
	ErlTermPtr<> tuplePattern = 
		new ErlTuple(new ErlVariable("Pid"),
		             new ErlVariable("Values")); 
	int counter = 0;
	try {
		while (1) {
			VariableBinding binding;
			ErlTermPtr<> receivedTerm(mailbox->receive());
			counter++;
			//std::cout << "Recibido: " << receivedTerm->toString() << std::endl;
			
			if (tuplePattern->match(receivedTerm.get(), &binding)) {	

				ErlPid *pid = ErlPid::cast(binding.search("Pid"));
				ErlList *values = ErlList::cast(binding.search("Values"));
				
				ErlTermPtr<ErlList> newList;
				if (values->arity() == 0) {
					newList.reset(new ErlEmptyList());
				} else {
					ErlConsList *newConsList = new ErlConsList();
					newList.reset(newConsList);
					for (unsigned int i = 0; i<values->arity(); i++) { 
						ErlLong *value = 
							ErlLong::cast(values->elementAt(i));
						newConsList->addElement(					
							new ErlLong(c.increment(value->longValue())));
					}
					newConsList->close();
				}
				mailbox->send(pid, newList.get());
				
			} else {
				std::cout << "Error: incorrect format" << std::endl;
				std::cout << "Levamos " << counter << std::endl;
			}
				
		}
	} catch (EpiException &e) {
		std::cout << "Error: " << e.getMessage();
	}
}
void test4_2(MailBox *mailbox){
}

int main(int argc, char **argv) {
	bool use_auto = false;
	if (argc < 6) {
		std::cout << "Use: " << argv[0] << 
			" <local node name> <remote node name> <cookie> <auto|self> <nºtest>" <<
			std::endl;
		exit(0);
	}
    
	LOCALNODE = argv[1];
	REMOTENODE = argv[2];
	COOKIE = argv[3];
	if (strcmp("auto", argv[4]) == 0) {
		use_auto = true;
	}
	int test_number = atoi(argv[5]);
	
    if (argc > 6 && argv[6][0] == '1') {
	    Debug( dc::notice.on() );
	    Debug( dc::erlang.on() );
	    //Debug( dc::erlang_warning.on() );
	    Debug( dc::connect.on() );
	    Debug( libcw_do.on() );
    }

	LocalNode *node;
	MailBox* mailbox;
	try {
        std::cout << "Starting node " << LOCALNODE << "\n";
	
		if (use_auto) {
			AutoNode *node2 = new AutoNode(LOCALNODE, COOKIE);
			node = node2;
			if (!node2->ping(REMOTENODE, 3000)) {
				std::cout << "Can't connect to "<< REMOTENODE << std::endl;
				return 1;
			}
			mailbox	= node2->createMailBox();
			node2->registerMailBox("perf", mailbox);	
		} else {
			node = new LocalNode(LOCALNODE, COOKIE);
			node->publishPort();
			Connection *connection = node->connect(REMOTENODE);
//			Connection *connection = node->accept();
			mailbox = node->createMailBox(connection);
			connection->start();
		}
		
		switch (test_number) {
		case 1: 
			test1(mailbox);
			break;
		case 2: 
			test2(mailbox);
			break;
		case 3: 
			test3(mailbox);
			break;
		case 4: 
			test4(mailbox);
			break;
		case 21: 
			test1_2(mailbox);
			break;
		case 22: 
			test2_2(mailbox);
			break;
		case 23: 
			test3_2(mailbox);
			break;
		case 24: 
			test4_2(mailbox);
			break;
		}
    } catch (EpiException &e) {
        std::cout << "Catched exception: " << e.getMessage() << "\n";
        exit(1);
    }
	

	
	return 0;
	
}
