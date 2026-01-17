typedef long long __m64 __attribute__((__vector_size__(16), __aligned__(8)));

int main() {
    __m64 foo;
    foo[0] = 2;
    foo[1] = 3;
    return 0;
}
