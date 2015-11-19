#!/bin/sh

set -e

SCRIPT=$(readlink $0 || true)
if [ -z $SCRIPT ]; then
    SCRIPT=$0
fi;
SCRIPT_DIR="$(cd `dirname "$SCRIPT"` && pwd -P)"


CURL_BIN=`which curl`
if ! test -n "CURLBIN"; then
    echo "Error: curl is required. Add it to 'PATH'"
    exit 1
fi

CERTIFI_URL=https://mkcert.org/generate/
CA_BUNDLE=cacerts.pem
CA_SRC=$SCRIPT_DIR/src/certifi_pemcerts.erl.src
CA_OUT=$SCRIPT_DIR/src/certifi_pemcerts.erl

curl -o $CA_BUNDLE $CERTIFI_URL

cat $CA_SRC \
    | head -n `grep -n "%% GENERATED" $CA_SRC | cut -d : -f 1` \
    > $CA_OUT
sed -e '/^#/d' $CA_BUNDLE >> $CA_OUT
cat $CA_SRC \
    | tail -n +`grep -n "%% GENERATED" $CA_SRC | cut -d : -f 1`  \
    >> $CA_OUT
mv $CA_BUNDLE $SCRIPT_DIR/priv/
