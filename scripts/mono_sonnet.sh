#!/bin/bash
for INPUT in "$@"
do
stack exec -- seq-sel $INPUT --config=poem-config/mono_sonnet.yaml \
> output/$INPUT-monoson.txt
cat output/$INPUT-monoson.txt
done
