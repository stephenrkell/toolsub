#include <array>
#include <iostream>

using std::cout;
using std::endl;

// borrowed from stack overflow!
// https://stackoverflow.com/questions/5839357/detect-operator-support-with-decltype-sfinae

template <class T>
struct supports_less_than
{
    // a lambda whose return type is the type of constructing a single char
    // *after* a comma operator that evaluates the "<" operator on type U
    template <class U>
    static auto less_than_test(const U* u) -> decltype(*u < *u, char(0))
    { }

    // a static member function that returns a length-2 thing
    static std::array<char, 2> less_than_test(...) { }

    static const bool value = (sizeof(less_than_test((T*)0)) == 1);
};
template <class T>
struct supports_operator_less_than
{
    // a lambda whose return type is the type of constructing a single char
    // *after* a comma operator that evaluates the "<" operator on type U
    template <class U>
    static auto less_than_test(const U* u) -> decltype(operator<(*u, *u), char(0))
    { }

    // a static member function that returns a length-2 thing
    static std::array<char, 2> less_than_test(...) { }

    static const bool value = (sizeof(less_than_test((T*)0)) == 1);
};

int main()
{
    std::cout << "Does std::string support '<'? " << std::boolalpha << supports_less_than<std::string>::value << endl;
    std::cout << "Does 'int' support '<'? " << std::boolalpha << supports_less_than<int>::value << endl;
    std::cout << "Does std::string support 'operator<'? " << std::boolalpha << supports_operator_less_than<std::string>::value << endl;
    std::cout << "Does 'int' support 'operator<'? " << std::boolalpha << supports_operator_less_than<int>::value << endl;
}
