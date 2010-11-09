/*
 * DO NOT EDIT.  THIS FILE IS GENERATED FROM test/ITestErlXPCOMComponent.idl
 */

#ifndef __gen_ITestErlXPCOMComponent_h__
#define __gen_ITestErlXPCOMComponent_h__


#ifndef __gen_nsISupports_h__
#include "nsISupports.h"
#endif

#ifndef __gen_nsIClassInfo_h__
#include "nsIClassInfo.h"
#endif

/* For IDL files that don't want to include root IDL files. */
#ifndef NS_NO_VTABLE
#define NS_NO_VTABLE
#endif

/* starting interface:    ITestErlXPCOMComponent */
#define ITESTERLXPCOMCOMPONENT_IID_STR "37a524b7-e2ed-4b35-8560-0636148f5282"

#define ITESTERLXPCOMCOMPONENT_IID \
  {0x37a524b7, 0xe2ed, 0x4b35, \
    { 0x85, 0x60, 0x06, 0x36, 0x14, 0x8f, 0x52, 0x82 }}

class NS_NO_VTABLE ITestErlXPCOMComponent : public nsISupports {
 public: 

  NS_DEFINE_STATIC_IID_ACCESSOR(ITESTERLXPCOMCOMPONENT_IID)

  /* attribute AString one_string; */
  NS_IMETHOD GetOne_string(nsAString & aOne_string) = 0;
  NS_IMETHOD SetOne_string(const nsAString & aOne_string) = 0;

  /* readonly attribute AString other_string; */
  NS_IMETHOD GetOther_string(nsAString & aOther_string) = 0;

  /* void PrintStringArray (in PRUint32 count, [array, size_is (count)] in string valueArray); */
  NS_IMETHOD PrintStringArray(PRUint32 count, const char **valueArray) = 0;

  /* void GetStrings (out PRUint32 count, [array, size_is (count), retval] out string str); */
  NS_IMETHOD GetStrings(PRUint32 *count, char ***str) = 0;

  /* void aMethod (); */
  NS_IMETHOD AMethod(void) = 0;

  /* void testInBool (in boolean v); */
  NS_IMETHOD TestInBool(PRBool v) = 0;

  /* void testOctet (in octet v); */
  NS_IMETHOD TestOctet(PRUint8 v) = 0;

  /* void testShort (in short v); */
  NS_IMETHOD TestShort(PRInt16 v) = 0;

  /* void testLong (in long v); */
  NS_IMETHOD TestLong(PRInt32 v) = 0;

  /* void testLongLong (in long long v); */
  NS_IMETHOD TestLongLong(PRInt64 v) = 0;

  /* void testUnsignedShort (in unsigned short v); */
  NS_IMETHOD TestUnsignedShort(PRUint16 v) = 0;

  /* void testUnsignedLong (in unsigned long v); */
  NS_IMETHOD TestUnsignedLong(PRUint32 v) = 0;

  /* void testUnsignedLongLong (in unsigned long long v); */
  NS_IMETHOD TestUnsignedLongLong(PRUint64 v) = 0;

  /* void testFloat (in float v); */
  NS_IMETHOD TestFloat(float v) = 0;

  /* void testDouble (in double v); */
  NS_IMETHOD TestDouble(double v) = 0;

  /* void testChar (in char v); */
  NS_IMETHOD TestChar(char v) = 0;

  /* void testWChar (in wchar v); */
  NS_IMETHOD TestWChar(PRUnichar v) = 0;

  /* void testAll (in boolean v_bool, in octet v_octet, in short v_short, in long v_long, in long long v_longlong, in unsigned short v_ushort, in unsigned long v_ulong, in unsigned long long v_ulonglong, in float v_float, in double v_double, in char v_char, in wchar v_wchar); */
  NS_IMETHOD TestAll(PRBool v_bool, PRUint8 v_octet, PRInt16 v_short, PRInt32 v_long, PRInt64 v_longlong, PRUint16 v_ushort, PRUint32 v_ulong, PRUint64 v_ulonglong, float v_float, double v_double, char v_char, PRUnichar v_wchar) = 0;

  /* void testString (in string s); */
  NS_IMETHOD TestString(const char *s) = 0;

  /* void testWString (in wstring s); */
  NS_IMETHOD TestWString(const PRUnichar *s) = 0;

  /* void testStringSizeIs (in unsigned long count, [size_is (count)] in string str); */
  NS_IMETHOD TestStringSizeIs(PRUint32 count, const char *str) = 0;

  /* void testIID (in nsIIDRef iid); */
  NS_IMETHOD TestIID(const nsIID & iid) = 0;

  /* void testISupports (in ITestErlXPCOMComponent obj); */
  NS_IMETHOD TestISupports(ITestErlXPCOMComponent *obj) = 0;

  /* void testISupportsIID ([iid_is (iid)] in ITestErlXPCOMComponent obj, in nsIIDRef iid); */
  NS_IMETHOD TestISupportsIID(ITestErlXPCOMComponent *obj, const nsIID & iid) = 0;

  /* boolean testOutBool (in boolean v); */
  NS_IMETHOD TestOutBool(PRBool v, PRBool *_retval) = 0;

  /* octet testOutOctet (in octet v); */
  NS_IMETHOD TestOutOctet(PRUint8 v, PRUint8 *_retval) = 0;

  /* short testOutShort (in short v); */
  NS_IMETHOD TestOutShort(PRInt16 v, PRInt16 *_retval) = 0;

  /* long testOutLong (in long v); */
  NS_IMETHOD TestOutLong(PRInt32 v, PRInt32 *_retval) = 0;

  /* unsigned short testOutUnsignedShort (in unsigned short v); */
  NS_IMETHOD TestOutUnsignedShort(PRUint16 v, PRUint16 *_retval) = 0;

  /* unsigned long testOutUnsignedLong (in unsigned long v); */
  NS_IMETHOD TestOutUnsignedLong(PRUint32 v, PRUint32 *_retval) = 0;

  /* float testOutFloat (in float v); */
  NS_IMETHOD TestOutFloat(float v, float *_retval) = 0;

  /* double testOutDouble (in double v); */
  NS_IMETHOD TestOutDouble(double v, double *_retval) = 0;

  /* char testOutChar (in char v); */
  NS_IMETHOD TestOutChar(char v, char *_retval) = 0;

  /* wchar testOutWChar (in wchar v); */
  NS_IMETHOD TestOutWChar(PRUnichar v, PRUnichar *_retval) = 0;

  /* string testOutString (); */
  NS_IMETHOD TestOutString(char **_retval) = 0;

  /* wstring testOutWString (); */
  NS_IMETHOD TestOutWString(PRUnichar **_retval) = 0;

  /* void testInOutAll (inout boolean v_bool, inout octet v_octet, inout short v_short, inout long v_long, inout unsigned short v_ushort, inout unsigned long v_ulong, inout float v_float, inout double v_double, inout char v_char, inout wchar v_wchar); */
  NS_IMETHOD TestInOutAll(PRBool *v_bool, PRUint8 *v_octet, PRInt16 *v_short, PRInt32 *v_long, PRUint16 *v_ushort, PRUint32 *v_ulong, float *v_float, double *v_double, char *v_char, PRUnichar *v_wchar) = 0;

  /* ITestErlXPCOMComponent testOutISupports (); */
  NS_IMETHOD TestOutISupports(ITestErlXPCOMComponent **_retval) = 0;

};

/* Use this macro when declaring classes that implement this interface. */
#define NS_DECL_ITESTERLXPCOMCOMPONENT \
  NS_IMETHOD GetOne_string(nsAString & aOne_string); \
  NS_IMETHOD SetOne_string(const nsAString & aOne_string); \
  NS_IMETHOD GetOther_string(nsAString & aOther_string); \
  NS_IMETHOD PrintStringArray(PRUint32 count, const char **valueArray); \
  NS_IMETHOD GetStrings(PRUint32 *count, char ***str); \
  NS_IMETHOD AMethod(void); \
  NS_IMETHOD TestInBool(PRBool v); \
  NS_IMETHOD TestOctet(PRUint8 v); \
  NS_IMETHOD TestShort(PRInt16 v); \
  NS_IMETHOD TestLong(PRInt32 v); \
  NS_IMETHOD TestLongLong(PRInt64 v); \
  NS_IMETHOD TestUnsignedShort(PRUint16 v); \
  NS_IMETHOD TestUnsignedLong(PRUint32 v); \
  NS_IMETHOD TestUnsignedLongLong(PRUint64 v); \
  NS_IMETHOD TestFloat(float v); \
  NS_IMETHOD TestDouble(double v); \
  NS_IMETHOD TestChar(char v); \
  NS_IMETHOD TestWChar(PRUnichar v); \
  NS_IMETHOD TestAll(PRBool v_bool, PRUint8 v_octet, PRInt16 v_short, PRInt32 v_long, PRInt64 v_longlong, PRUint16 v_ushort, PRUint32 v_ulong, PRUint64 v_ulonglong, float v_float, double v_double, char v_char, PRUnichar v_wchar); \
  NS_IMETHOD TestString(const char *s); \
  NS_IMETHOD TestWString(const PRUnichar *s); \
  NS_IMETHOD TestStringSizeIs(PRUint32 count, const char *str); \
  NS_IMETHOD TestIID(const nsIID & iid); \
  NS_IMETHOD TestISupports(ITestErlXPCOMComponent *obj); \
  NS_IMETHOD TestISupportsIID(ITestErlXPCOMComponent *obj, const nsIID & iid); \
  NS_IMETHOD TestOutBool(PRBool v, PRBool *_retval); \
  NS_IMETHOD TestOutOctet(PRUint8 v, PRUint8 *_retval); \
  NS_IMETHOD TestOutShort(PRInt16 v, PRInt16 *_retval); \
  NS_IMETHOD TestOutLong(PRInt32 v, PRInt32 *_retval); \
  NS_IMETHOD TestOutUnsignedShort(PRUint16 v, PRUint16 *_retval); \
  NS_IMETHOD TestOutUnsignedLong(PRUint32 v, PRUint32 *_retval); \
  NS_IMETHOD TestOutFloat(float v, float *_retval); \
  NS_IMETHOD TestOutDouble(double v, double *_retval); \
  NS_IMETHOD TestOutChar(char v, char *_retval); \
  NS_IMETHOD TestOutWChar(PRUnichar v, PRUnichar *_retval); \
  NS_IMETHOD TestOutString(char **_retval); \
  NS_IMETHOD TestOutWString(PRUnichar **_retval); \
  NS_IMETHOD TestInOutAll(PRBool *v_bool, PRUint8 *v_octet, PRInt16 *v_short, PRInt32 *v_long, PRUint16 *v_ushort, PRUint32 *v_ulong, float *v_float, double *v_double, char *v_char, PRUnichar *v_wchar); \
  NS_IMETHOD TestOutISupports(ITestErlXPCOMComponent **_retval); 

/* Use this macro to declare functions that forward the behavior of this interface to another object. */
#define NS_FORWARD_ITESTERLXPCOMCOMPONENT(_to) \
  NS_IMETHOD GetOne_string(nsAString & aOne_string) { return _to GetOne_string(aOne_string); } \
  NS_IMETHOD SetOne_string(const nsAString & aOne_string) { return _to SetOne_string(aOne_string); } \
  NS_IMETHOD GetOther_string(nsAString & aOther_string) { return _to GetOther_string(aOther_string); } \
  NS_IMETHOD PrintStringArray(PRUint32 count, const char **valueArray) { return _to PrintStringArray(count, valueArray); } \
  NS_IMETHOD GetStrings(PRUint32 *count, char ***str) { return _to GetStrings(count, str); } \
  NS_IMETHOD AMethod(void) { return _to AMethod(); } \
  NS_IMETHOD TestInBool(PRBool v) { return _to TestInBool(v); } \
  NS_IMETHOD TestOctet(PRUint8 v) { return _to TestOctet(v); } \
  NS_IMETHOD TestShort(PRInt16 v) { return _to TestShort(v); } \
  NS_IMETHOD TestLong(PRInt32 v) { return _to TestLong(v); } \
  NS_IMETHOD TestLongLong(PRInt64 v) { return _to TestLongLong(v); } \
  NS_IMETHOD TestUnsignedShort(PRUint16 v) { return _to TestUnsignedShort(v); } \
  NS_IMETHOD TestUnsignedLong(PRUint32 v) { return _to TestUnsignedLong(v); } \
  NS_IMETHOD TestUnsignedLongLong(PRUint64 v) { return _to TestUnsignedLongLong(v); } \
  NS_IMETHOD TestFloat(float v) { return _to TestFloat(v); } \
  NS_IMETHOD TestDouble(double v) { return _to TestDouble(v); } \
  NS_IMETHOD TestChar(char v) { return _to TestChar(v); } \
  NS_IMETHOD TestWChar(PRUnichar v) { return _to TestWChar(v); } \
  NS_IMETHOD TestAll(PRBool v_bool, PRUint8 v_octet, PRInt16 v_short, PRInt32 v_long, PRInt64 v_longlong, PRUint16 v_ushort, PRUint32 v_ulong, PRUint64 v_ulonglong, float v_float, double v_double, char v_char, PRUnichar v_wchar) { return _to TestAll(v_bool, v_octet, v_short, v_long, v_longlong, v_ushort, v_ulong, v_ulonglong, v_float, v_double, v_char, v_wchar); } \
  NS_IMETHOD TestString(const char *s) { return _to TestString(s); } \
  NS_IMETHOD TestWString(const PRUnichar *s) { return _to TestWString(s); } \
  NS_IMETHOD TestStringSizeIs(PRUint32 count, const char *str) { return _to TestStringSizeIs(count, str); } \
  NS_IMETHOD TestIID(const nsIID & iid) { return _to TestIID(iid); } \
  NS_IMETHOD TestISupports(ITestErlXPCOMComponent *obj) { return _to TestISupports(obj); } \
  NS_IMETHOD TestISupportsIID(ITestErlXPCOMComponent *obj, const nsIID & iid) { return _to TestISupportsIID(obj, iid); } \
  NS_IMETHOD TestOutBool(PRBool v, PRBool *_retval) { return _to TestOutBool(v, _retval); } \
  NS_IMETHOD TestOutOctet(PRUint8 v, PRUint8 *_retval) { return _to TestOutOctet(v, _retval); } \
  NS_IMETHOD TestOutShort(PRInt16 v, PRInt16 *_retval) { return _to TestOutShort(v, _retval); } \
  NS_IMETHOD TestOutLong(PRInt32 v, PRInt32 *_retval) { return _to TestOutLong(v, _retval); } \
  NS_IMETHOD TestOutUnsignedShort(PRUint16 v, PRUint16 *_retval) { return _to TestOutUnsignedShort(v, _retval); } \
  NS_IMETHOD TestOutUnsignedLong(PRUint32 v, PRUint32 *_retval) { return _to TestOutUnsignedLong(v, _retval); } \
  NS_IMETHOD TestOutFloat(float v, float *_retval) { return _to TestOutFloat(v, _retval); } \
  NS_IMETHOD TestOutDouble(double v, double *_retval) { return _to TestOutDouble(v, _retval); } \
  NS_IMETHOD TestOutChar(char v, char *_retval) { return _to TestOutChar(v, _retval); } \
  NS_IMETHOD TestOutWChar(PRUnichar v, PRUnichar *_retval) { return _to TestOutWChar(v, _retval); } \
  NS_IMETHOD TestOutString(char **_retval) { return _to TestOutString(_retval); } \
  NS_IMETHOD TestOutWString(PRUnichar **_retval) { return _to TestOutWString(_retval); } \
  NS_IMETHOD TestInOutAll(PRBool *v_bool, PRUint8 *v_octet, PRInt16 *v_short, PRInt32 *v_long, PRUint16 *v_ushort, PRUint32 *v_ulong, float *v_float, double *v_double, char *v_char, PRUnichar *v_wchar) { return _to TestInOutAll(v_bool, v_octet, v_short, v_long, v_ushort, v_ulong, v_float, v_double, v_char, v_wchar); } \
  NS_IMETHOD TestOutISupports(ITestErlXPCOMComponent **_retval) { return _to TestOutISupports(_retval); } 

/* Use this macro to declare functions that forward the behavior of this interface to another object in a safe way. */
#define NS_FORWARD_SAFE_ITESTERLXPCOMCOMPONENT(_to) \
  NS_IMETHOD GetOne_string(nsAString & aOne_string) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetOne_string(aOne_string); } \
  NS_IMETHOD SetOne_string(const nsAString & aOne_string) { return !_to ? NS_ERROR_NULL_POINTER : _to->SetOne_string(aOne_string); } \
  NS_IMETHOD GetOther_string(nsAString & aOther_string) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetOther_string(aOther_string); } \
  NS_IMETHOD PrintStringArray(PRUint32 count, const char **valueArray) { return !_to ? NS_ERROR_NULL_POINTER : _to->PrintStringArray(count, valueArray); } \
  NS_IMETHOD GetStrings(PRUint32 *count, char ***str) { return !_to ? NS_ERROR_NULL_POINTER : _to->GetStrings(count, str); } \
  NS_IMETHOD AMethod(void) { return !_to ? NS_ERROR_NULL_POINTER : _to->AMethod(); } \
  NS_IMETHOD TestInBool(PRBool v) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestInBool(v); } \
  NS_IMETHOD TestOctet(PRUint8 v) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestOctet(v); } \
  NS_IMETHOD TestShort(PRInt16 v) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestShort(v); } \
  NS_IMETHOD TestLong(PRInt32 v) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestLong(v); } \
  NS_IMETHOD TestLongLong(PRInt64 v) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestLongLong(v); } \
  NS_IMETHOD TestUnsignedShort(PRUint16 v) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestUnsignedShort(v); } \
  NS_IMETHOD TestUnsignedLong(PRUint32 v) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestUnsignedLong(v); } \
  NS_IMETHOD TestUnsignedLongLong(PRUint64 v) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestUnsignedLongLong(v); } \
  NS_IMETHOD TestFloat(float v) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestFloat(v); } \
  NS_IMETHOD TestDouble(double v) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestDouble(v); } \
  NS_IMETHOD TestChar(char v) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestChar(v); } \
  NS_IMETHOD TestWChar(PRUnichar v) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestWChar(v); } \
  NS_IMETHOD TestAll(PRBool v_bool, PRUint8 v_octet, PRInt16 v_short, PRInt32 v_long, PRInt64 v_longlong, PRUint16 v_ushort, PRUint32 v_ulong, PRUint64 v_ulonglong, float v_float, double v_double, char v_char, PRUnichar v_wchar) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestAll(v_bool, v_octet, v_short, v_long, v_longlong, v_ushort, v_ulong, v_ulonglong, v_float, v_double, v_char, v_wchar); } \
  NS_IMETHOD TestString(const char *s) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestString(s); } \
  NS_IMETHOD TestWString(const PRUnichar *s) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestWString(s); } \
  NS_IMETHOD TestStringSizeIs(PRUint32 count, const char *str) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestStringSizeIs(count, str); } \
  NS_IMETHOD TestIID(const nsIID & iid) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestIID(iid); } \
  NS_IMETHOD TestISupports(ITestErlXPCOMComponent *obj) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestISupports(obj); } \
  NS_IMETHOD TestISupportsIID(ITestErlXPCOMComponent *obj, const nsIID & iid) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestISupportsIID(obj, iid); } \
  NS_IMETHOD TestOutBool(PRBool v, PRBool *_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestOutBool(v, _retval); } \
  NS_IMETHOD TestOutOctet(PRUint8 v, PRUint8 *_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestOutOctet(v, _retval); } \
  NS_IMETHOD TestOutShort(PRInt16 v, PRInt16 *_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestOutShort(v, _retval); } \
  NS_IMETHOD TestOutLong(PRInt32 v, PRInt32 *_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestOutLong(v, _retval); } \
  NS_IMETHOD TestOutUnsignedShort(PRUint16 v, PRUint16 *_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestOutUnsignedShort(v, _retval); } \
  NS_IMETHOD TestOutUnsignedLong(PRUint32 v, PRUint32 *_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestOutUnsignedLong(v, _retval); } \
  NS_IMETHOD TestOutFloat(float v, float *_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestOutFloat(v, _retval); } \
  NS_IMETHOD TestOutDouble(double v, double *_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestOutDouble(v, _retval); } \
  NS_IMETHOD TestOutChar(char v, char *_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestOutChar(v, _retval); } \
  NS_IMETHOD TestOutWChar(PRUnichar v, PRUnichar *_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestOutWChar(v, _retval); } \
  NS_IMETHOD TestOutString(char **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestOutString(_retval); } \
  NS_IMETHOD TestOutWString(PRUnichar **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestOutWString(_retval); } \
  NS_IMETHOD TestInOutAll(PRBool *v_bool, PRUint8 *v_octet, PRInt16 *v_short, PRInt32 *v_long, PRUint16 *v_ushort, PRUint32 *v_ulong, float *v_float, double *v_double, char *v_char, PRUnichar *v_wchar) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestInOutAll(v_bool, v_octet, v_short, v_long, v_ushort, v_ulong, v_float, v_double, v_char, v_wchar); } \
  NS_IMETHOD TestOutISupports(ITestErlXPCOMComponent **_retval) { return !_to ? NS_ERROR_NULL_POINTER : _to->TestOutISupports(_retval); } 

#if 0
/* Use the code below as a template for the implementation class for this interface. */

/* Header file */
class _MYCLASS_ : public ITestErlXPCOMComponent
{
public:
  NS_DECL_ISUPPORTS
  NS_DECL_ITESTERLXPCOMCOMPONENT

  _MYCLASS_();

private:
  ~_MYCLASS_();

protected:
  /* additional members */
};

/* Implementation file */
NS_IMPL_ISUPPORTS1(_MYCLASS_, ITestErlXPCOMComponent)

_MYCLASS_::_MYCLASS_()
{
  /* member initializers and constructor code */
}

_MYCLASS_::~_MYCLASS_()
{
  /* destructor code */
}

/* attribute AString one_string; */
NS_IMETHODIMP _MYCLASS_::GetOne_string(nsAString & aOne_string)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}
NS_IMETHODIMP _MYCLASS_::SetOne_string(const nsAString & aOne_string)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* readonly attribute AString other_string; */
NS_IMETHODIMP _MYCLASS_::GetOther_string(nsAString & aOther_string)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void PrintStringArray (in PRUint32 count, [array, size_is (count)] in string valueArray); */
NS_IMETHODIMP _MYCLASS_::PrintStringArray(PRUint32 count, const char **valueArray)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void GetStrings (out PRUint32 count, [array, size_is (count), retval] out string str); */
NS_IMETHODIMP _MYCLASS_::GetStrings(PRUint32 *count, char ***str)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void aMethod (); */
NS_IMETHODIMP _MYCLASS_::AMethod()
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void testInBool (in boolean v); */
NS_IMETHODIMP _MYCLASS_::TestInBool(PRBool v)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void testOctet (in octet v); */
NS_IMETHODIMP _MYCLASS_::TestOctet(PRUint8 v)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void testShort (in short v); */
NS_IMETHODIMP _MYCLASS_::TestShort(PRInt16 v)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void testLong (in long v); */
NS_IMETHODIMP _MYCLASS_::TestLong(PRInt32 v)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void testLongLong (in long long v); */
NS_IMETHODIMP _MYCLASS_::TestLongLong(PRInt64 v)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void testUnsignedShort (in unsigned short v); */
NS_IMETHODIMP _MYCLASS_::TestUnsignedShort(PRUint16 v)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void testUnsignedLong (in unsigned long v); */
NS_IMETHODIMP _MYCLASS_::TestUnsignedLong(PRUint32 v)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void testUnsignedLongLong (in unsigned long long v); */
NS_IMETHODIMP _MYCLASS_::TestUnsignedLongLong(PRUint64 v)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void testFloat (in float v); */
NS_IMETHODIMP _MYCLASS_::TestFloat(float v)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void testDouble (in double v); */
NS_IMETHODIMP _MYCLASS_::TestDouble(double v)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void testChar (in char v); */
NS_IMETHODIMP _MYCLASS_::TestChar(char v)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void testWChar (in wchar v); */
NS_IMETHODIMP _MYCLASS_::TestWChar(PRUnichar v)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void testAll (in boolean v_bool, in octet v_octet, in short v_short, in long v_long, in long long v_longlong, in unsigned short v_ushort, in unsigned long v_ulong, in unsigned long long v_ulonglong, in float v_float, in double v_double, in char v_char, in wchar v_wchar); */
NS_IMETHODIMP _MYCLASS_::TestAll(PRBool v_bool, PRUint8 v_octet, PRInt16 v_short, PRInt32 v_long, PRInt64 v_longlong, PRUint16 v_ushort, PRUint32 v_ulong, PRUint64 v_ulonglong, float v_float, double v_double, char v_char, PRUnichar v_wchar)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void testString (in string s); */
NS_IMETHODIMP _MYCLASS_::TestString(const char *s)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void testWString (in wstring s); */
NS_IMETHODIMP _MYCLASS_::TestWString(const PRUnichar *s)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void testStringSizeIs (in unsigned long count, [size_is (count)] in string str); */
NS_IMETHODIMP _MYCLASS_::TestStringSizeIs(PRUint32 count, const char *str)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void testIID (in nsIIDRef iid); */
NS_IMETHODIMP _MYCLASS_::TestIID(const nsIID & iid)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void testISupports (in ITestErlXPCOMComponent obj); */
NS_IMETHODIMP _MYCLASS_::TestISupports(ITestErlXPCOMComponent *obj)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void testISupportsIID ([iid_is (iid)] in ITestErlXPCOMComponent obj, in nsIIDRef iid); */
NS_IMETHODIMP _MYCLASS_::TestISupportsIID(ITestErlXPCOMComponent *obj, const nsIID & iid)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* boolean testOutBool (in boolean v); */
NS_IMETHODIMP _MYCLASS_::TestOutBool(PRBool v, PRBool *_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* octet testOutOctet (in octet v); */
NS_IMETHODIMP _MYCLASS_::TestOutOctet(PRUint8 v, PRUint8 *_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* short testOutShort (in short v); */
NS_IMETHODIMP _MYCLASS_::TestOutShort(PRInt16 v, PRInt16 *_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* long testOutLong (in long v); */
NS_IMETHODIMP _MYCLASS_::TestOutLong(PRInt32 v, PRInt32 *_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* unsigned short testOutUnsignedShort (in unsigned short v); */
NS_IMETHODIMP _MYCLASS_::TestOutUnsignedShort(PRUint16 v, PRUint16 *_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* unsigned long testOutUnsignedLong (in unsigned long v); */
NS_IMETHODIMP _MYCLASS_::TestOutUnsignedLong(PRUint32 v, PRUint32 *_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* float testOutFloat (in float v); */
NS_IMETHODIMP _MYCLASS_::TestOutFloat(float v, float *_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* double testOutDouble (in double v); */
NS_IMETHODIMP _MYCLASS_::TestOutDouble(double v, double *_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* char testOutChar (in char v); */
NS_IMETHODIMP _MYCLASS_::TestOutChar(char v, char *_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* wchar testOutWChar (in wchar v); */
NS_IMETHODIMP _MYCLASS_::TestOutWChar(PRUnichar v, PRUnichar *_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* string testOutString (); */
NS_IMETHODIMP _MYCLASS_::TestOutString(char **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* wstring testOutWString (); */
NS_IMETHODIMP _MYCLASS_::TestOutWString(PRUnichar **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* void testInOutAll (inout boolean v_bool, inout octet v_octet, inout short v_short, inout long v_long, inout unsigned short v_ushort, inout unsigned long v_ulong, inout float v_float, inout double v_double, inout char v_char, inout wchar v_wchar); */
NS_IMETHODIMP _MYCLASS_::TestInOutAll(PRBool *v_bool, PRUint8 *v_octet, PRInt16 *v_short, PRInt32 *v_long, PRUint16 *v_ushort, PRUint32 *v_ulong, float *v_float, double *v_double, char *v_char, PRUnichar *v_wchar)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* ITestErlXPCOMComponent testOutISupports (); */
NS_IMETHODIMP _MYCLASS_::TestOutISupports(ITestErlXPCOMComponent **_retval)
{
    return NS_ERROR_NOT_IMPLEMENTED;
}

/* End of implementation class template. */
#endif


#endif /* __gen_ITestErlXPCOMComponent_h__ */
