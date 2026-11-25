#!/bin/sh
# Build MMM natively with gfortran.
#   local gfortran:  ./native/build.sh
#   via Docker:      docker run --rm -v "$PWD/native":/w -w /w gcc:14 ./build.sh
set -e
cd "$(dirname "$0")"
python3 prep.py 2>/dev/null || python prep.py
cd build
# -std=legacy -fdec -fdec-char-conversions : MS Fortran extensions (text in INTEGER*2)
# -fno-align-commons                      : packed COMMON layout like MS Fortran
# -fno-automatic -finit-local-zero         : static, zeroed locals like DOS
# -ffixed-line-length-72 -fno-pad-source : prep.py pads to col 72 itself
gfortran -std=legacy -fdec -fdec-char-conversions -fno-range-check \
  -ffixed-line-length-72 -fno-pad-source -fno-align-commons \
  -fno-automatic -finit-local-zero -w -O1 -g -static \
  -o mmm ../compat.f90 ../compat77.f MMM.FOR MMLB.FOR MMS?.FOR
echo "built native/build/mmm"
