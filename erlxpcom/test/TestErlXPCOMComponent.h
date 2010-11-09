#ifndef _TESTERLXPCOMCOMPONENT_H
#define _TESTERLXPCOMCOMPONENT_H

#include <nsCOMPtr.h>
#include "ITestErlXPCOMComponent.h"

/** Based on ITheSample.h template */
class TestErlXPCOMComponent : public ITestErlXPCOMComponent
{
public:
  NS_DECL_ISUPPORTS
  NS_DECL_ITESTERLXPCOMCOMPONENT
  TestErlXPCOMComponent();
  ~TestErlXPCOMComponent();

private:

	nsCOMPtr<ITestErlXPCOMComponent> object;

protected:
  /* additional members */
};


#endif
