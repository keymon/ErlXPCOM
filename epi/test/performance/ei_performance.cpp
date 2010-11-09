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

#include <ei.h>
#include <iostream>
#include <sys/time.h>
#include <unistd.h>
#include <string>
#include <string.h>

/**************************************************************************/
long miliseconds_mark;

void cronometer_start() {
	struct timeval tp;
	gettimeofday(&tp, NULL);
	miliseconds_mark = tp.tv_sec*1000 + tp.tv_usec/1000;
}

long cronometer_stop() {
	struct timeval tp;
	gettimeofday(&tp, NULL);
	return (tp.tv_sec*1000 + tp.tv_usec/1000) - miliseconds_mark;
}
/**************************************************************************/

ei_cnode ec;
int connection_fd;
erlang_pid local_pid;
#define REMOTE_SERVER_NAME "reply_server"

int initialize_ei(std::string localhostname,
				  std::string remotenodename,
			      std::string cookie) {
    int ei_res;
	std::string localhostname2;
	std::string alivename;
	std::string hostname;

	std::string::size_type pos = localhostname.find ("@",0);

    if (pos == std::string::npos) {
        alivename = localhostname;
        hostname = "localhost";
    } else {
        alivename = localhostname.substr(0, pos);
        hostname = localhostname.substr(pos+1, localhostname.size());
    }

	localhostname2 = alivename+"@"+hostname;

	std::cout << "nodename = " << localhostname2 << std::endl <<
			     "alivename = " << alivename << std::endl <<
			     "hostname = " << hostname << std::endl;

	ei_res = ei_connect_xinit(&ec,  
							  hostname.c_str(),
							  alivename.c_str(), 
							  localhostname2.c_str(), 
							  NULL, cookie.c_str(), 0);
							  
	if (ei_res < 0) {
		std::cout << "Failed initializing node" << std::endl;
		return ei_res;
	}

	strcpy(local_pid.node, localhostname2.c_str());
	local_pid.num = 0;
	local_pid.serial = 0;
	local_pid.creation = 0;
 	
	connection_fd = ei_connect(&ec, (char *) remotenodename.c_str());
	
	if (connection_fd< 0) {
		std::cout << "Failed connecting remote node " << std::endl;
		return -1;
	}

	return 0;
		
}

int finish_ei() {
	close(connection_fd);
	return 0;
}

/* tests de tipos simples */
int test_performance_atom(char *atom, int times) {
	
	ei_x_buff output;
	ei_x_buff input;
	erlang_msg msg;
	int version;
	int ei_res;
	int index;
	char decoded_atom[MAXATOMLEN]; 
	
	// Inicializa buffers
	ei_x_new_with_version(&output);
	ei_x_new(&input);
	
	// Codifica
	ei_x_encode_tuple_header(&output, 2);
	ei_x_encode_pid(&output, &local_pid);
	ei_x_encode_atom(&output, atom);
	
	for (int i = 0; i<times; i++) {
		if (ei_reg_send(&ec, connection_fd, REMOTE_SERVER_NAME, 
			output.buff, output.index) < 0) {
				return 1;
		}
		do {
			ei_res = ei_xreceive_msg(connection_fd, &msg, &input);
		} while(ei_res == ERL_TICK);
		
		if (ei_res == ERL_ERROR) {
			return -1;
		}
		index = 0;
		if (ei_decode_version(input.buff, &index, &version) < 0) {
			std::cout << "failed decoding version \n";
			return 1;
		}
				

		if (ei_decode_atom(input.buff, &index, decoded_atom) < 0) {
			std::cout << "failed decoding atom \n";
			return 1;
		}		
	}
		
	return 0;
}

int main(int argc, char** argv) {
		
	if (initialize_ei(argv[1], argv[2], argv[3]) != 0) {
		std::cout << "Init failed" << std::endl;
		return 1;
	}
	
	long ms;
	
	cronometer_start();
	
	if (test_performance_atom("hello",1000) != 0) {
		std::cout << "test failed\n";
	}
	
	ms = cronometer_stop();
	
	std::cout << "tardou "<< ms << "\n";
	
	finish_ei();

	return 0;
}


