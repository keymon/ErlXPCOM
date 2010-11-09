#include <string>
#include <iostream>

#include <ei.h>

#include <string.h>

#include "SillyClass.hpp"

std::string LOCALNODE;
std::string REMOTENODE;
std::string COOKIE;

void test1(int fd) {
	SillyClass c;

    erlang_msg msg;
	ei_x_buff buff;
	ei_x_buff buff2;
	int counter = 0;
	
	int type;
	int size;
	int arity;
	int version;
	int res;
	
	while (1) {
		ei_x_new(&buff);
		ei_x_new_with_version(&buff2);
		
		do {
			res = ei_xreceive_msg(fd,  &msg, &buff);
			if (res < 0) goto on_error;
		} while (res == ERL_TICK) ;

		counter++;	
		
		if (msg.msgtype == ERL_SEND || msg.msgtype ==  ERL_REG_SEND) {
			int index = 0;
			if (ei_decode_version(buff.buff, &index, &version) < 0) goto on_error;
			if (ei_get_type(buff.buff, &index, &type, &size) < 0) goto on_error;
			
			if (type == ERL_SMALL_TUPLE_EXT || type == ERL_LARGE_TUPLE_EXT) {
				if (ei_decode_tuple_header(buff.buff, &index, &arity) < 0)
					goto on_error;
				if (arity!=2) {
					std::cout << "Error bad message (bad tuple arity)" << std::endl;
					return;
				}
				erlang_pid pid;
				if (ei_decode_pid(buff.buff, &index, &pid) < 0) 
					goto on_error;
				long long value;
				if (ei_decode_longlong(buff.buff, &index, &value) < 0) 
					goto on_error;
				
				long long newvalue = c.increment(value);
				
				if (ei_x_encode_longlong(&buff2, newvalue) <0) 
					goto on_error;
				
				if (ei_send(fd, &pid, buff2.buff, buff2.index) <0)
					goto on_error;
			} else {
				std::cout << "Error bad message" << std::endl;
				
				std::cout << "Levamos " << counter << std::endl;
				return;
			}
		}
		
		ei_x_free(&buff);
		ei_x_free(&buff2);
	}
	
	return;
on_error:
	std::cout << "Error receiving message" << std::endl;
	return;
}

void test2(int fd) {
	SillyClass c;

    erlang_msg msg;
	ei_x_buff buff;
	ei_x_buff buff2;
	int counter = 0;
	
	int type;
	int size;
	int arity;
	int version;
	int res;
	
	while (1) {
		ei_x_new(&buff);
		ei_x_new_with_version(&buff2);
		
		do {
			res = ei_xreceive_msg(fd,  &msg, &buff);
			if (res < 0) goto on_error;
		} while (res == ERL_TICK) ;

		counter++;	
		
		if (msg.msgtype == ERL_SEND || msg.msgtype ==  ERL_REG_SEND) {
			int index = 0;
			if (ei_decode_version(buff.buff, &index, &version) < 0) goto on_error;
			if (ei_get_type(buff.buff, &index, &type, &size) < 0) goto on_error;
			
			if (type == ERL_SMALL_TUPLE_EXT || type == ERL_LARGE_TUPLE_EXT) {
				if (ei_decode_tuple_header(buff.buff, &index, &arity) < 0)
					goto on_error;
				if (arity!=3) {
					std::cout << "Error bad message (bad tuple arity)" << std::endl;
					return;
				}
				erlang_pid pid;
				if (ei_decode_pid(buff.buff, &index, &pid) < 0) 
					goto on_error;
				char str[100];
				if (ei_decode_string(buff.buff, &index, str) < 0) 
					goto on_error;
				std::string str1(str);
				if (ei_decode_string(buff.buff, &index, str) < 0) 
					goto on_error;
				std::string str2(str);
				
				std::string newstr = c.concatenate(str1, str2);
				
				if (ei_x_encode_string(&buff2, newstr.c_str()) < 0) 
					goto on_error;
				
				if (ei_send(fd, &pid, buff2.buff, buff2.index) <0)
					goto on_error;
			} else {
				std::cout << "Error bad message" << std::endl;
				
				std::cout << "Levamos " << counter << std::endl;
				return;
			}
		}
		
		ei_x_free(&buff);
		ei_x_free(&buff2);
	}
	
	return;
on_error:
	std::cout << "Error receiving message" << std::endl;
	return;
}

void test3(int fd) {
	SillyClass c;

    erlang_msg msg;
	ei_x_buff buff;
	ei_x_buff buff2;
	int counter = 0;
	
	int type;
	int size;
	int arity;
	int version;
	int res;
	
	while (1) {
		ei_x_new(&buff);
		ei_x_new_with_version(&buff2);
		
		do {
			res = ei_xreceive_msg(fd,  &msg, &buff);
			if (res < 0) goto on_error;
		} while (res == ERL_TICK) ;

		counter++;	
		
		if (msg.msgtype == ERL_SEND || msg.msgtype ==  ERL_REG_SEND) {
			int index = 0;
			if (ei_decode_version(buff.buff, &index, &version) < 0) goto on_error;
			if (ei_get_type(buff.buff, &index, &type, &size) < 0) goto on_error;
			
			if (type == ERL_SMALL_TUPLE_EXT || type == ERL_LARGE_TUPLE_EXT) {
				if (ei_decode_tuple_header(buff.buff, &index, &arity) < 0)
					goto on_error;
				
				char atom[100];
				if (ei_decode_atom(buff.buff, &index, atom) < 0) 
					goto on_error;
				// Check what message is
				if (strcmp("incr", atom) == 0 && arity == 3) {
					erlang_pid pid;
					if (ei_decode_pid(buff.buff, &index, &pid) < 0) 
						goto on_error;
					long long value;
					if (ei_decode_longlong(buff.buff, &index, &value) < 0) 
						goto on_error;
					
					long long newvalue = c.increment(value);
					
					if (ei_x_encode_longlong(&buff2, newvalue) <0) 
						goto on_error;
					
					if (ei_send(fd, &pid, buff2.buff, buff2.index) <0)
						goto on_error;
				} else if (strcmp("concat", atom) == 0 && arity == 4) {
				
					erlang_pid pid;
					if (ei_decode_pid(buff.buff, &index, &pid) < 0) 
						goto on_error;
					char str[100];
					if (ei_decode_string(buff.buff, &index, str) < 0) 
						goto on_error;
					std::string str1(str);
					if (ei_decode_string(buff.buff, &index, str) < 0) 
						goto on_error;
					std::string str2(str);
					
					std::string newstr = c.concatenate(str1, str2);
					
					if (ei_x_encode_string(&buff2, newstr.c_str()) < 0) 
						goto on_error;
					
					if (ei_send(fd, &pid, buff2.buff, buff2.index) <0)
						goto on_error;
				} else {
					std::cout << "Unknown message" << std::endl;
				}
			} else {
				std::cout << "Error bad message" << std::endl;
				std::cout << "Levamos " << counter << std::endl;
			}
		}
		
		ei_x_free(&buff);
		ei_x_free(&buff2);
	}
	
	return;
on_error:
	std::cout << "Error receiving message" << std::endl;
	return;
}

void test4(int fd) {
	SillyClass c;

    erlang_msg msg;
	ei_x_buff buff;
	ei_x_buff buff2;
	int counter = 0;
	
	int type;
	int size;
	int arity;
	int version;
	int res;
	
	while (1) {
		ei_x_new(&buff);
		ei_x_new_with_version(&buff2);
		
		do {
			res = ei_xreceive_msg(fd,  &msg, &buff);
			if (res < 0) goto on_error;
		} while (res == ERL_TICK) ;

		counter++;	
		
		if (msg.msgtype == ERL_SEND || msg.msgtype ==  ERL_REG_SEND) {
			int index = 0;
			if (ei_decode_version(buff.buff, &index, &version) < 0) goto on_error;
			if (ei_get_type(buff.buff, &index, &type, &size) < 0) goto on_error;
			
			if (type == ERL_SMALL_TUPLE_EXT || type == ERL_LARGE_TUPLE_EXT) {
				if (ei_decode_tuple_header(buff.buff, &index, &arity) < 0)
					goto on_error;
				if (arity!=2) {
					std::cout << "Error bad message (bad tuple arity)" << std::endl;
					return;
				}
				erlang_pid pid;
				if (ei_decode_pid(buff.buff, &index, &pid) < 0) 
					goto on_error;
				
				if (ei_get_type(buff.buff, &index, &type, &size) < 0) goto on_error;
				switch (type) {
					case ERL_NIL_EXT: 
						if (ei_decode_list_header(buff.buff, &index, &size) < 0) 
							goto on_error;
						break;
					
					case ERL_LIST_EXT:
						if (ei_decode_list_header(buff.buff, &index, &size) < 0) 
							goto on_error;
						if (ei_x_encode_list_header(&buff2, size) < 0) 
							goto on_error;
						for (int i=0; i<size; i++) {
							long long value;
							ei_decode_longlong(buff.buff, &index, &value);
							long long newvalue = c.increment(value);
					
							if (ei_x_encode_longlong(&buff2, newvalue) <0) 
								goto on_error;
						}
						ei_x_encode_empty_list(&buff2);
						break;
					case ERL_STRING_EXT:
						char *str = new char[size+1];
						if (ei_decode_string(buff.buff, &index, str) < 0) 
							goto on_error;
						if (ei_x_encode_list_header(&buff2, size) < 0) 
							goto on_error;
						for (int i=0; i<size; i++) {
							long long newvalue = c.increment(str[i]);
							if (ei_x_encode_longlong(&buff2, newvalue) <0) 
								goto on_error;
						}
						ei_x_encode_empty_list(&buff2);
						break;
				}
				if (ei_send(fd, &pid, buff2.buff, buff2.index) <0)
					goto on_error;
			} else {
				std::cout << "Error bad message" << std::endl;
				
				std::cout << "Levamos " << counter << std::endl;
				return;
			}
		}
		
		ei_x_free(&buff);
		ei_x_free(&buff2);
	}
	
	return;
on_error:
	std::cout << "Error receiving message" << std::endl;
	return;
}

int main(int argc, char **argv) {
	if (argc < 5) {
		std::cout << "Use: " << argv[0] << 
			" <local node name> <remote node name> <cookie>  <nºtest>" <<
			std::endl;
		exit(0);
	}
    
	LOCALNODE = argv[1];
	REMOTENODE = argv[2];
	COOKIE = argv[3];
	int test_number = atoi(argv[4]);
	
	/////////////////////////////
	
    std::string nodename = LOCALNODE;
	std::string alivename;
    std::string hostname;
    
	std::string::size_type pos = nodename.find ("@",0);

    if (pos == std::string::npos) {
        alivename = nodename;
        hostname = "defaulthost";
    } else {
        alivename = nodename.substr(0, pos);
        hostname = nodename.substr(pos+1, nodename.size());
    }
	
	ei_cnode ec;
    int ei_res =
            ei_connect_xinit(&ec,
                             hostname.c_str(),
                             alivename.c_str(),
                             nodename.c_str(),
                             // FIXME: In ei_interface °°ipaddr is NOT USED!!
                             // DIOXXXXXXXXXXX!!!! }:-/
                             NULL,
                             COOKIE.c_str(),
                             // FIXME: Please! how does creation work!?!?!
                             0);
    if (ei_res < 0) {
		std::cout << "Error creating node" << std::endl;
		return 1;
    }

	// Conectando!!!
	int fd = ei_connect(&ec, (char*) REMOTENODE.c_str());
    if (fd < 0) {
		std::cout << "Error connecting remote" << std::endl;
		return 1;
    }
	
	switch (test_number) {
	case 1: 
		test1(fd);
		break;
	case 2: 
		test2(fd);
		break;
	case 3: 
		test3(fd);
		break;
	case 4: 
		test4(fd);
		break;
	}
	
	return 0;
}