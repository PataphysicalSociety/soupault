#!/bin/sh

FILE=$1
VERSION=$2
REPO_DIR=$3

function usage () {
    echo "Usage: $0 <soupault executable> <version> <repo dir>"
    exit 1
}

if [ "$FILE" == "" ] || [ "$VERSION" == "" ] || [ "$REPO_DIR" == "" ]; then
    usage
fi

if [ ! -f $FILE ]; then
    echo "File $FILE does not exist"
    exit 1
fi

if [ ! -f $FILE ]; then
    echo "Soupault repository directory $REPO_DIR does not exist"
    exit 1
fi

# Determine the executable target OS and architecture
FILE_TYPE=$(file $FILE)

if $(echo $FILE_TYPE | grep "ELF 64-bit LSB executable" 2>&1 >/dev/null); then
    RELEASE_OS=linux
    RELEASE_ARCH=x86_64
elif $(echo $FILE_TYPE | grep "PE32+ executable" 2>&1 >/dev/null); then
    RELEASE_OS=win64
    RELEASE_ARCH=""
elif $(echo $FILE_TYPE | grep "Mach-O 64-bit x86_64 executable" 2>&1 >/dev/null); then
    RELEASE_OS="macos"
    RELEASE_ARCH="x86_64"
else
    echo "Unsupported executable architecture!"
    exit 1
fi

# Make a release string, like soupault-4.0.1-linux-x86_64
RELEASE=soupault-$VERSION-$RELEASE_OS
if [ -n "$RELEASE_ARCH" ]; then
    RELEASE=$RELEASE-$RELEASE_ARCH
fi

# Make a release archive
echo "Packaging release $RELEASE"

mkdir $RELEASE

if [ "$RELEASE_OS" == "win64" ]; then
    RELEASE_ARCHIVE=$RELEASE.zip

    cp $FILE $RELEASE/soupault.exe

    dos2unix -n $REPO_DIR/README.md $RELEASE/README.TXT
    dos2unix -n $REPO_DIR/LICENSE $RELEASE/LICENSE.TXT

    zip -r $RELEASE_ARCHIVE $RELEASE
else
    RELEASE_ARCHIVE=$RELEASE.tar.gz

    cp $FILE $RELEASE/soupault
    chmod +x $RELEASE/soupault

    cp $REPO_DIR/README.md $RELEASE/
    cp $REPO_DIR/LICENSE $RELEASE/

    tar cvfz $RELEASE_ARCHIVE $RELEASE
fi

# Checksum and sign the archive
sha256sum $RELEASE_ARCHIVE >> sha256sums
minisign -Sm $RELEASE_ARCHIVE

