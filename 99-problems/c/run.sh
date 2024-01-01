#!/bin/bash

set -e

gcc -Wall -Wextra lists.c -o /tmp/lists && /tmp/lists
