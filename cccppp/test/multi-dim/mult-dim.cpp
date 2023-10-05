#include <iostream>
using namespace std;

int main() {
  int b[1];
  b[0] = 1;
  int a[1][2];
  a[0][0] = 1;
  a[0][1] = 7;
  cout << a[0][b[0]] << endl;
}
