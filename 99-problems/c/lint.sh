#!/bin/bash

#set -e

echo -e "\033[1;34m=> Running clang-tidy...\033[1;m"
clang-tidy ./lists.c -- -Wall -Wextra

echo -e "\033[1;34m=> Running splint...\033[1;m"
splint ./lists.c

echo -e "\033[1;34m=> Running cppcheck...\033[1;m"
cppcheck .

echo "---"
