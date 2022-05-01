#!/bin/bash

ifort -o cefine cefine.f90

mv cefine test
cd test
./cefine  --msc_exc 3-5,9,102,15,600,6-9 --func b3lyp
