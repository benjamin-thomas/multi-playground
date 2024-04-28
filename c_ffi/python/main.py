#!/usr/bin/env python3

import ctypes
import sys


class Argon2:
    libargon2 = ctypes.CDLL('libargon2.so')

    _argon2id_verify = libargon2.argon2id_verify
    _argon2id_verify.argtypes = [ctypes.c_char_p,
                                 ctypes.POINTER(ctypes.c_char), ctypes.c_size_t]
    _argon2id_verify.restype = ctypes.c_int

    @staticmethod
    def verify_password(encoded_hash, password):
        password_ptr = ctypes.create_string_buffer(password.encode('utf-8'))
        password_len = len(password)

        print("Password size:", password_len)
        result = Argon2._argon2id_verify(
            encoded_hash.encode('utf-8'), password_ptr, password_len)
        print("Verify result:", result)

        return result == 0


# Example usage
encoded_hash = "$argon2id$v=19$m=16,t=2,p=1$c2FsdC02Nzg$XUb4CH1RppZ2CLAi36TUrw"

if len(sys.argv) < 2:
    print("Usage: python3 main.py <password>")
    exit(1)

password = sys.argv[1]
if Argon2.verify_password(encoded_hash, password):
    print("Password is correct!")
else:
    print("Password is incorrect!")
