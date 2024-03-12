
#include <gtest/gtest.h>
#include <iostream>
using namespace std;

/*
echo ./solution.cpp | entr -c bash -c 'g++ -o /tmp/tmp ./solution.cpp &&
/tmp/tmp'
*/

#define red "\x1b[31m"
#define green "\x1b[32m"
#define blue "\x1b[34m"
#define reset "\x1b[0m"

void assertEqual(string a, string b) {
  if (a == b) {
    cout << green << "🟢 PASS: " << a << reset << endl;
  } else {
    cout << red << "🔴 FAIL: " << a << " != " << b << reset << endl;
  }
}

string interleaveLetters(string a, string b) {
  int longest = max(a.length(), b.length());
  string result = "";
  for (int i = 0; i < longest; i++) {
    if (i < a.length()) {
      result += a[i];
    }
    if (i < b.length()) {
      result += b[i];
    }
  }
  return result;
}

int main(int argc, char **argv) {

  assertEqual("ABCD", interleaveLetters("AC", "BD"));
  assertEqual("ABCD_EF", interleaveLetters("AC_EF", "BD"));
  assertEqual("ABCD_EF", interleaveLetters("AC", "BD_EF"));
  assertEqual("ABC", interleaveLetters("ABC", ""));
  assertEqual("ABC", interleaveLetters("", "ABC"));
  assertEqual("", interleaveLetters("", ""));

  return 0;
}
