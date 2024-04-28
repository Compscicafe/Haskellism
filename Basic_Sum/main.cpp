#include <iostream>
using namespace std;

int foo1(int a)
{
    a +=2;
    return a;
}
int foo2(int* a)
{
    *a += 2;
    return *a;
}

int foo3(int &a)
{
    a += 2;
    return a;
}
int main()
{
    return 0;
}
