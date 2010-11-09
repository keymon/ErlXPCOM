#include "MiniCppUnit.hxx"

class MyTests : public TestFixture<MyTests>
{
public:
	 TEST_FIXTURE( MyTests ) {
		  TEST_CASE( test );
		  TEST_CASE( test );
		  TEST_CASE( test );
	 }
	 void test() {
		  ASSERT_EQUALS( 4, 1+1+2 );
	 }
};
REGISTER_FIXTURE( MyTests )