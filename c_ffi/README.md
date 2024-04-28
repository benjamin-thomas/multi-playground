# Interfacing with the argon2 library

Hash generated with: https://argon2.online

```
password=hello
salt=salt-678
hash=$argon2id$v=19$m=16,t=2,p=1$c2FsdC02Nzg$XUb4CH1RppZ2CLAi36TUrw
```


## Requirements
```sh
# Dependency
apt show libargon2-dev

# Print symbols
readelf -Ws /usr/lib/x86_64-linux-gnu/libargon2.so.1

# Header file, for documentation
/usr/include/argon2.h
```

## Public API
```c
ARGON2_PUBLIC int argon2id_verify
( const   char *encoded
, const   void *pwd
, const size_t  pwdlen
);
```

---

## Examples

```
ruby ruby/main.rb hello
python3 ./python/main.py hello

cd ./ocaml/
dune exec ./main.exe hello

cd ./ada/
gnatmake -c argon2.ads && gnatmake -c ./main.adb && gnatbind ./main.ali && gnatlink -largon2 ./main.ali && ./main hello

cd ./c/
gcc main.c -o /tmp/tmp -l argon2 && /tmp/tmp hello
```