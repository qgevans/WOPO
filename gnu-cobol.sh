#!/bin/sh
args=`getopt d $*`
if [ $? -ne 0 ]; then
    echo 'Usage: sh gnu-cobol.sh [-d]'
    exit 2
fi
set -- $args
while true; do
    case "$1" in
    -d)
        DEBUG=1
        shift
        ;;
    --)
        shift
        break
        ;;
    esac
done
${CC:-cc} ${DEBUG:+-DDEBUG} -o channel.o -c channel.c
${COBC:-cobc} ${DEBUG:+-fdebugging-line} -x WOPO-CNF.COB PRINTCNF.COB
${COBC:-cobc} ${DEBUG:+-fdebugging-line} -x WOPO.COB IRC-MSG.COB PRINTCNF.COB DECASCII.COB ENCASCII.COB BF-RUN.COB channel.o

