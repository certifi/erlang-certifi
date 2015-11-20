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
CA_BUNDLE=$SCRIPT_DIR/priv/cacerts.pem
CA_SRC=$SCRIPT_DIR/src/certifi_cacerts.erl.src
CA_OUT=$SCRIPT_DIR/src/certifi_cacerts.erl


WEAK_SRC=$SCRIPT_DIR/src/certifi_weak.erl.src
WEAK_BUNDLE=$SCRIPT_DIR/priv/weak.pem
WEAK_OUT=$SCRIPT_DIR/src/certifi_weak.erl

OLD_ROOT_BUNDLE=$SCRIPT_DIR/priv/old_root.pem

mkcert()
{
    BUNDLE=$1
    SRC=$2
    OUT=$3

    echo "==> generate $BUNDLE"
    cat $SRC | head -n `grep -n "%% GENERATED" $SRC | cut -d : -f 1` > $OUT
    sed -e '/^#/d' $BUNDLE >> $OUT
    cat $SRC | tail -n +`grep -n "%% GENERATED" $SRC | cut -d : -f 1` >> $OUT
}


# fetch last stable bundle
curl -o $CA_BUNDLE $CERTIFI_URL
# build weak cacert
cat $CA_BUNDLE $OLD_ROOT_BUNLE > $WEAK_BUNDLE

mkcert $CA_BUNDLE $CA_SRC $CA_OUT
mkcert $WEAK_BUNDLE $WEAK_SRC $WEAK_OUT
