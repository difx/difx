#include <iostream>
#include "dirlist_parameter.h"
#include "dirlist_exception.h"

int main(int argc, char **argv)
{
	DirListParameter *P1;

	P1 = new DirListParameter; P1->print(); delete P1;
	P1 = new DirListParameter("x", "y"); P1->print(); delete P1;
	P1 = new DirListParameter("x2", "y2", "c2"); P1->print(); delete P1;
	P1 = new DirListParameter;
	P1->setKey("setKey");
	P1->setValue("setValue");
	P1->setComment("setComment");
	P1->print();
	try
	{
		std::cout << "Int[100] = " << P1->getInt(100) << std::endl;
	}
	catch (DirListException &e)
	{
		std::cout << "Exception caught: " << e.what() << std::endl;
	}
	delete P1;
	P1 = new DirListParameter("x3", "y3,z3, w3, t3 , u3", "c3"); P1->print(); delete P1;
	P1 = new DirListParameter("x4", "'sterntor'"); P1->print(); delete P1;
	P1 = new DirListParameter("x5", "'1,2,3,4,5'"); P1->print(); delete P1;
	P1 = new DirListParameter("x6", "'1','2','3','4','5'"); P1->print(); delete P1;

	return 0;
}
