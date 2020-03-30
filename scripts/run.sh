#!/bin/bash
for INPUT in "$@"
do
stack exec -- seq-sel $INPUT --config=poem-config/default_poem.yaml \
> output/$INPUT.txt
cat output/$INPUT.txt
done
