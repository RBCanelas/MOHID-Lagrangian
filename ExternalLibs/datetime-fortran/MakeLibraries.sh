#!/bin/bash

dirout=build

# "dirout" is created to store results or it is cleaned if it already exists
if [ -e $dirout ]; then
  rm -f -r $dirout
fi
mkdir $dirout

# building initial libs
cd $dirout
echo ------------------------------------
echo Running cmake for datetime-fortran library
cmake -Wno-dev .. -DCMAKE_BUILD_TYPE=Release
echo ------------------------------------
echo Running make for datetime-fortran library
make

cd ..
cd ..