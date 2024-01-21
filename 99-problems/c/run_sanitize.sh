#!/bin/bash

set -e

set -x
gcc -Wall -Wextra lists.c -o /tmp/lists -fsanitize=address && /tmp/lists_sanitize
