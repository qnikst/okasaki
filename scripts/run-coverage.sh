#!/bin/bash

ghc=$(which ghc)

test -d hpc-markup || mkdir hpc-markup

for h in *.hs ; do
    rm ${h/.hs/.tix}
    ghc -fhpc $h &&
    test -f ${h/.hs/} && ./${h/.hs/} &&
    hpc markup --srcdir=. ${h/.hs/.tix} --destdir=hpc-markup/${h} > /dev/null &&
    hpc report --srcdir=. ${h/.hs/.tix} 
done

