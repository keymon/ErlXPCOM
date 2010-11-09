#include <string>

class SillyClass {
public:
	inline int increment(int i) {
		return i+1;
	}
	
	inline std::string concatenate(std::string str1, std::string str2) {
		return str1+str2;
	}
};