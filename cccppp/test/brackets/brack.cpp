#include <iostream>
using namespace std;

const char* getString() {
  return "Hello :)";
}
int main() {
  
  int arr[1];
  arr[0] = 1;
  cout << arr [ 0 ] << endl;
  return getString()[0];
}
