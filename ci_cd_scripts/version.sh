#!/bin/bash

LATEST_TAG=`git describe --abbrev=0 --tags --always`

BASE_VERSION=$LATEST_TAG

DISTANCE=`git rev-list --count ${LATEST_TAG}..HEAD`

if [[ "$DISTANCE" == "0" ]]; then
    VERSION=$BASE_VERSION
else
    SHORT=`git rev-parse --short=8 HEAD`
    VERSION="${BASE_VERSION}.dev${DISTANCE}-${SHORT}"
fi

echo $VERSION
