#!/usr/bin/env bash -x

ormolu --mode inplace $(git ls-files '*.hs')
