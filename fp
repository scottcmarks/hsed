#!/bin/bash
cd ~/hsed
find app src test -type f -name '*.lhs' -or -name '*.hs' -not -name '*.th.hs' -exec grep -nH "$*" {} ';'
