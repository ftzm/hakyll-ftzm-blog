#!/usr/bin/env bash

git --no-pager log --format=%B -n 1 \
    | tr "\n" " " \
    | sed -r "s/.*release:\s?(patch|minor|major).*/\1/"
