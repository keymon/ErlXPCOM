#include <Config.hpp>

#include <iostream>
#include <memory>
#include <string>
#include <unistd.h>

#include <nsIGenericFactory.h> 
#include <prmem.h>
#include <nsMemory.h>
#include <nsLiteralString.h>
#include "TestErlXPCOMComponent.h"

//#include "RequestTransport.h"
//#include "ErlangOrb.h"
//#include "ErlXPCOMInit.h"
// using namespace erlxpcom;


/***************************************************************************
 Based on ITheSample.h template
***************************************************************************/
// We will implement the class info in order to use this class in the 
// xpcshell from different threads. 
// Realmente no es thread safe, pero lo definimos así para poder probar :)
NS_IMPL_ISUPPORTS1_CI(TestErlXPCOMComponent, ITestErlXPCOMComponent)
NS_DECL_CLASSINFO(TestErlXPCOMComponent)

/***************************************************************************/
/** Component load code */

// Create the generic factory
NS_GENERIC_FACTORY_CONSTRUCTOR(TestErlXPCOMComponent);
 
// List of components in this module
static const nsModuleComponentInfo components[] = 
{ 
	 { "TestErlXPCOMComponent: ErlXPCOM test component",
		ITESTERLXPCOMCOMPONENT_IID,
		"@lfcia.org/TestErlXPCOMComponent",
		TestErlXPCOMComponentConstructor, 
		NULL, // registrationProc
		NULL, // unregistrationProc
		NULL, // factory destructor
		NS_CI_INTERFACE_GETTER_NAME(TestErlXPCOMComponent),
		NULL, // language helper
		&NS_CLASSINFO_NAME(TestErlXPCOMComponent),
		nsIClassInfo::THREADSAFE
	 }
}; 
 
nsresult
 TestModuleConstructor(nsIModule *self) {
	return NS_OK;
}

NS_IMPL_NSGETMODULE_WITH_CTOR(TestErlXPCOMComponentModule, 
							  components, TestModuleConstructor)
/***************************************************************************/



TestErlXPCOMComponent::TestErlXPCOMComponent()
{
	 fprintf(stderr, "DBG: Starting the object TestErlXPCOMComponent\n");
}

TestErlXPCOMComponent::~TestErlXPCOMComponent()
{
	 fprintf(stderr, "DBG: Destroing TestErlXPCOMComponent\n");
}

//ErlXPCOMInit *erlXPCOMInit = 0;
//
//nsresult initErlXPCOM(nsIComponentManager *componentManager) {
//	if (erlXPCOMInit) {
//		return true;
//	}
//	try {
//		
//		erlXPCOMInit = new ErlXPCOMInit();
//		erlXPCOMInit->launchErlang();
//		
//		nsresult rv;
//		nsIServiceManager *serviceManager;
//
//		if (NS_FAILED(rv = NS_GetServiceManager(&serviceManager))) {
//			throw XPCOMException("Can't get service manager", rv);
//		}
//			
//		erlXPCOMInit->init(componentManager, serviceManager);
//		
//		return NS_OK;
//		
//	} catch (ErlXPCOMException e) {
//		PR_LOG(ErlXPCOMLog::getLog(), PR_LOG_ERROR, 
//			("Can't initialize ErlXPCOM, Exception: %s", 
//			 e.getMessage().c_str()));
//		delete erlXPCOMInit;
//		erlXPCOMInit = 0;
//		return NS_ERROR_FAILURE;
//	}
//}
//
///* void Init (in string localnodename, in string remotenodename, in string cookie, in boolean debug); */
//NS_IMETHODIMP TestErlXPCOMComponent::Init(const char *localnodename, const char *remotenodename, const char *cookie, PRBool debug)
//{
//
//	nsCOMPtr<nsIComponentManager> componentManager;
//	NS_GetComponentManager(getter_AddRefs(componentManager));
//
//    if(NS_FAILED(initErlXPCOM(componentManager.get()))) {
//        return NS_ERROR_FACTORY_NOT_LOADED;
//    }
//    return NS_OK;
//}

/* readonly attribute AString other_string; */
NS_IMETHODIMP TestErlXPCOMComponent::GetOther_string(nsAString & aOther_string)
{
	aOther_string.Assign(NS_LITERAL_STRING("hola tio"));
    return NS_OK;
}


/* attribute AString one_string; */
NS_IMETHODIMP TestErlXPCOMComponent::GetOne_string(nsAString & aOne_string)
{
	aOne_string.Assign(NS_LITERAL_STRING("hola tio"));
	return NS_OK;
}
NS_IMETHODIMP TestErlXPCOMComponent::SetOne_string(const nsAString & aOne_string)
{
	char* buffer = ToNewCString(aOne_string);
	std::cout << "Pon one_string a: \"" << buffer << "\"" << std::endl;
	
    return NS_OK;
}


/* void aMethod (); */
NS_IMETHODIMP TestErlXPCOMComponent::AMethod()
{
	std::cout << "Hola mundo desde TestErlXPCOMComponent::AMethod()\n";
    return NS_OK;
}

/* void testInBool (in boolean v); */
NS_IMETHODIMP TestErlXPCOMComponent::TestInBool(PRBool v)
{
	std::cout << "Hola desde TestErlXPCOMComponent::TestInBool("<< v <<")\n";
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void testOctet (in octet v); */
NS_IMETHODIMP TestErlXPCOMComponent::TestOctet(PRUint8 v)
{
	std::cout << "Hola desde TestErlXPCOMComponent::TestOctet("<< v <<")\n";
    return NS_OK;
}

/* void testShort (in short v); */
NS_IMETHODIMP TestErlXPCOMComponent::TestShort(PRInt16 v)
{
	std::cout << "Hola desde TestErlXPCOMComponent::TestShort("<< v <<")\n";
    return NS_OK;
}

/* void testLong (in long v); */
NS_IMETHODIMP TestErlXPCOMComponent::TestLong(PRInt32 v)
{
	std::cout << "Hola desde TestErlXPCOMComponent::TestLong("<< v <<")\n";
    return NS_OK;
}

/* void testLongLong (in long long v); */
NS_IMETHODIMP TestErlXPCOMComponent::TestLongLong(PRInt64 v)
{
	std::cout << "Hola desde TestErlXPCOMComponent::TestLongLong("<< v <<")\n";
    return NS_OK;
}

/* void testUnsignedShort (in unsigned short v); */
NS_IMETHODIMP TestErlXPCOMComponent::TestUnsignedShort(PRUint16 v)
{
	std::cout << "Hola desde TestErlXPCOMComponent::TestUnsignedShort("<< v <<")\n";
    return NS_OK;
}

/* void testUnsignedLong (in unsigned long v); */
NS_IMETHODIMP TestErlXPCOMComponent::TestUnsignedLong(PRUint32 v)
{
	std::cout << "Hola desde TestErlXPCOMComponent::TestUnsignedLong("<< v <<")\n";
    return NS_OK;
}

/* void testUnsignedLongLong (in unsigned long long v); */
NS_IMETHODIMP TestErlXPCOMComponent::TestUnsignedLongLong(PRUint64 v)
{
	std::cout << "Hola desde TestErlXPCOMComponent::TestUnsignedLongLong("<< v <<")\n";
    return NS_OK;
}
/* void PrintStringArray (in PRUint32 count, [array, size_is (count)] in string valueArray); */
NS_IMETHODIMP TestErlXPCOMComponent::PrintStringArray(PRUint32 count, const char **valueArray)
{
	std::cout<< "Hola desde PrintStringArray:" << std::endl;
	for (unsigned int i=0; i< count; i++) {
		std::cout<< "\t " << i << ": " << valueArray[i] << std::endl;
	} 
	return NS_OK;
}

/* void GetStrings (out PRUint32 count, [array, size_is (count), retval] out string str); */
NS_IMETHODIMP TestErlXPCOMComponent::GetStrings(PRUint32 *count, char ***str)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void testFloat (in float v); */
NS_IMETHODIMP TestErlXPCOMComponent::TestFloat(float v)
{
	std::cout << "Hola desde TestErlXPCOMComponent::TestFloat("<< v <<")\n";
    return NS_OK;
}

/* void testDouble (in double v); */
NS_IMETHODIMP TestErlXPCOMComponent::TestDouble(double v)
{
	std::cout << "Hola desde TestErlXPCOMComponent::TestDouble("<< v <<")\n";
    return NS_OK;
}

/* void testChar (in char v); */
NS_IMETHODIMP TestErlXPCOMComponent::TestChar(char v)
{
	std::cout << "Hola desde TestErlXPCOMComponent::TestChar("<< v <<")\n";
    return NS_OK;
}

/* void testWChar (in wchar v); */
NS_IMETHODIMP TestErlXPCOMComponent::TestWChar(PRUnichar v)
{
	std::cout << "Hola desde TestErlXPCOMComponent::TestWChar("<< v <<")\n";
    return NS_OK;
}

/* void testAll (in octet v_octet, in short v_short, in long v_long, in long long v_longlong, in unsigned short v_ushort, in unsigned long v_ulong, in unsigned long long v_ulonglong, in float v_float, in double v_double, in char v_char, in wchar v_wchar); */
NS_IMETHODIMP TestErlXPCOMComponent::TestAll(
	 PRBool v_bool, 
	 PRUint8 v_octet, 
	 PRInt16 v_short, 
	 PRInt32 v_long, 
	 PRInt64 v_longlong, 
	 PRUint16 v_ushort, 
	 PRUint32 v_ulong, 
	 PRUint64 v_ulonglong, 
	 float v_float, 
	 double v_double, 
	 char v_char, 
	 PRUnichar v_wchar)
{
	std::cout << "Hola desde TestErlXPCOMComponent::TestAll(...):\n";
	std::cout << "\tboolean: "<< v_bool <<"\n";
	std::cout << "\toctet: "<< v_octet <<"\n";
	std::cout << "\tshort:" << v_short <<"\n";
	std::cout << "\tlong: " << v_long <<"\n";
	std::cout << "\tlong long: " << v_longlong <<"\n";
	std::cout << "\tunsigned short: " << v_ushort <<"\n";
	std::cout << "\tunsigned long: " << v_ulong <<"\n";
	std::cout << "\tunsigned long long: " << v_ulonglong <<"\n";
	std::cout << "\tfloat: " << v_float <<"\n";
	std::cout << "\tdouble: " << v_double <<"\n";
	std::cout << "\tchar: " << v_char <<"\n";
	std::cout << "\twchar:" << v_wchar <<"\n";
	
    return NS_OK;
}

/* void testString (in string s); */
NS_IMETHODIMP TestErlXPCOMComponent::TestString(const char *s)
{
	std::cout << "Hola desde TestErlXPCOMComponent::TestString("<< s <<")\n";
	return NS_OK;
}

/* void testWString (in wstring s); */
NS_IMETHODIMP TestErlXPCOMComponent::TestWString(const PRUnichar *s)
{
	std::cout << "Hola desde TestErlXPCOMComponent::TestWString("<< s <<")\n";
	return NS_OK;
}

/* void testStringSizeIs (in unsigned long count, [size_is (count)] in string str); */
NS_IMETHODIMP TestErlXPCOMComponent::TestStringSizeIs(PRUint32 count, const char *str)
{
	std::cout << "Hola desde TestErlXPCOMComponent::TestStringSizeIs("<< str <<"), size is " << count << " \n";
    return NS_OK;
}

/* void testIID (in nsIIDRef iid); */
NS_IMETHODIMP TestErlXPCOMComponent::TestIID(const nsIID & iid)
{
	char *iidstr = iid.ToString();
	std::cout << "Hola desde TestErlXPCOMComponent::TestIID("<< iidstr <<")\n";
	PR_Free(iidstr);
    return NS_OK;
}

/* void testISupports (in ITestErlXPCOMComponent obj); */
NS_IMETHODIMP TestErlXPCOMComponent::TestISupports(ITestErlXPCOMComponent *obj)
{
	std::cout << "Hola desde TestErlXPCOMComponent::TestISupports()\n";
	obj->AMethod();
    return NS_OK;
}

/* void testISupports2 ([iid_is (iid)] in ITestErlXPCOMComponent obj, in nsIIDRef iid); */
NS_IMETHODIMP TestErlXPCOMComponent::TestISupportsIID(ITestErlXPCOMComponent *obj, const nsIID & iid)
{
	char *iidstr = iid.ToString();
	std::cout << "Hola desde TestErlXPCOMComponent::TestISupports2("<< iidstr <<")\n";
	PR_Free(iidstr);
    return NS_OK;
}

/* boolean testOutBool (in boolean v); */
NS_IMETHODIMP TestErlXPCOMComponent::TestOutBool(PRBool v, PRBool *_retval)
{
	*_retval = !v;
    return NS_OK;
}

/* octet testOutOctet (in octet v); */
NS_IMETHODIMP TestErlXPCOMComponent::TestOutOctet(PRUint8 v, PRUint8 *_retval)
{
	*_retval = v+1;
    return NS_OK;
}

/* short testOutShort (in short v); */
NS_IMETHODIMP TestErlXPCOMComponent::TestOutShort(PRInt16 v, PRInt16 *_retval)
{
	*_retval = v+1;
    return NS_OK;
}

/* long testOutLong (in long v); */
NS_IMETHODIMP TestErlXPCOMComponent::TestOutLong(PRInt32 v, PRInt32 *_retval)
{
	*_retval = v+1;
    return NS_OK;
}

/* unsigned short testOutUnsignedShort (in unsigned short v); */
NS_IMETHODIMP TestErlXPCOMComponent::TestOutUnsignedShort(PRUint16 v, PRUint16 *_retval)
{
	*_retval = v+1;
    return NS_OK;
}

/* unsigned long testOutUnsignedLong (in unsigned long v); */
NS_IMETHODIMP TestErlXPCOMComponent::TestOutUnsignedLong(PRUint32 v, PRUint32 *_retval)
{
	*_retval = v+1;
    return NS_OK;
}

/* float testOutFloat (in float v); */
NS_IMETHODIMP TestErlXPCOMComponent::TestOutFloat(float v, float *_retval)
{
	*_retval = v+1;
    return NS_OK;
}

/* double testOutDouble (in double v); */
NS_IMETHODIMP TestErlXPCOMComponent::TestOutDouble(double v, double *_retval)
{
	*_retval = v+1;
    return NS_OK;
}

/* char testOutChar (in char v); */
NS_IMETHODIMP TestErlXPCOMComponent::TestOutChar(char v, char *_retval)
{
	*_retval = v+1;
    return NS_OK;
}

/* wchar testOutWChar (in wchar v); */
NS_IMETHODIMP TestErlXPCOMComponent::TestOutWChar(PRUnichar v, PRUnichar *_retval)
{
	*_retval = v+1;
    return NS_OK;
}

/* void testInOutAll (inout boolean v_bool, inout octet v_octet, inout short v_short, inout long v_long, inout unsigned short v_ushort, inout unsigned long v_ulong, inout float v_float, inout double v_double, inout char v_char, inout wchar v_wchar); */
NS_IMETHODIMP TestErlXPCOMComponent::TestInOutAll(PRBool *v_bool, PRUint8 *v_octet, PRInt16 *v_short, PRInt32 *v_long, PRUint16 *v_ushort, PRUint32 *v_ulong, float *v_float, double *v_double, char *v_char, PRUnichar *v_wchar)
{
		*v_bool = !v_bool;
		(*v_octet)++;
		(*v_short)++; 
		(*v_long)++;
//		(*v_longlong)++;
		(*v_ushort)++;
		(*v_ulong)++;
//		(*v_ulonglong)++;
		(*v_float)+=1;
		(*v_double)+=1;
		(*v_char)++;
		(*v_wchar)++;
		return NS_OK;
}

/* ITestErlXPCOMComponent testOutISupports (); */
NS_IMETHODIMP TestErlXPCOMComponent::TestOutISupports(ITestErlXPCOMComponent **_retval)
{
	*_retval = new TestErlXPCOMComponent();
    return NS_OK;
}

/* void testOutISupports2 (out nsIIDRef iid, [iid_is (iid)] out nsISupports obj); */
//NS_IMETHODIMP TestErlXPCOMComponent::TestOutISupportsIID(nsIID & *iid, nsISupports **obj)
//{
//	*iid = this->GetIID();
//	*obj = this;
//    return NS_OK;
//}

/* string testOutString (); */
NS_IMETHODIMP TestErlXPCOMComponent::TestOutString(char **_retval)
{
	char *str = "TestOutString";
	*_retval = (char*) nsMemory::Clone(str, sizeof(char)*(strlen(str)+1));
	return NS_OK;	
}

/* wstring testOutWString (); */
NS_IMETHODIMP TestErlXPCOMComponent::TestOutWString(PRUnichar **_retval)
{
	*_retval = ToNewUnicode(NS_LITERAL_CSTRING("TestOutWString"));
    return NS_OK;
}

/* nsIIDRef testOutIID (); */
//NS_IMETHODIMP TestErlXPCOMComponent::TestOutIID(nsIID & *_retval)
//{
//	*_retval = this->GetIID();
//    return NS_OK;
//}
//

