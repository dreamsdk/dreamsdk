#!/usr/bin/env bash

echo $1 | tr [:lower:] [:upper:] | sed 's/-/_/g'
