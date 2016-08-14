#!/bin/sh

virtualenv --python=`which python3.5` venv
./venv/bin/pip install django djangorestframework
ln -s ./venv/bin bin
