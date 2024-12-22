#!/bin/bash
./bh && git commit -am Update && stack build && stack install && hsed --version && git log -1 | cat
