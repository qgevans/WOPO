#!/bin/sh
BUILD_COBOL=1
BUILD_C=1
args=`getopt dCc $*`
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
    -C)
	unset BUILD_COBOL
	shift
	;;
    -c)
	unset BUILD_C
	shift
	;;
    --)
        shift
        break
        ;;
    esac
done
if [ $BUILD_C ]; then
    ${CC:-cc} ${DEBUG:+-DDEBUG} -o channel.o -c channel.c
fi
if [ $BUILD_COBOL ]; then
    ${COBC:-cobc} ${DEBUG:+-fdebugging-line} -std=mvs -x WOPO-CNF.COB PRINTCNF.COB
    ${COBC:-cobc} ${DEBUG:+-fdebugging-line} -std=mvs -A "-fbracket-depth=512" -x WOPO.COB IRC-MSG.COB PRINTCNF.COB DECASCII.COB ENCASCII.COB DECSTR.COB ENCSTR.COB BF-RUN.COB channel.o
fi

