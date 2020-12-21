#include <pybind11/pybind11.h>  // ここを追加

bool is_prime(long x) {
    for (long i = 2; i <= sqrt(x); i++) {
        if (x % i == 0)
          return false;
    }
    return true;
}

PYBIND11_MODULE(prime, m) {      // ここから
  m.def("is_prime", &is_prime);  //
}                                // ここまで追加
