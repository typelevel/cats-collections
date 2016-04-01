#!/bin/sh

openssl aes-256-cbc -K $encrypted_78d67e04f03e_key -iv $encrypted_78d67e04f03e_iv -in scripts/bintray.tar.gz.enc -out bintray.tar.gz -d

tar xzf -C $HOME bintray.tar.gz
