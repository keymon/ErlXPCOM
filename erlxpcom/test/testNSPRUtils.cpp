#include "NSPRUtils.h"
#include <unistd.h>
#include <iostream>

class ThreadTest: public NSPRThread {
public:
	virtual void run() {
		for (int i = 0; i<=10; i++) {
			std::cout << "Thread hello " << i << std::endl;
			sleep(1);
		}
	}
};

int main() {
	try {
		ThreadTest t;
		
		t.start();
		std::cout << "Sleeping for 5 seconds" << std::endl;
		sleep(5);	
		std::cout << "Waiting thread" << std::endl;
		t.join();
	} catch (std::exception &e) {
		std::cout << "Exception catched "<< e.what();
	}
	
}