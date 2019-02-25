#!/bin/zsh
set -ue

TEMPDIR=testtmp
BIN="./$(stack path --dist-dir)/build/hash2/hash2"

function assert() {
    set +ue
    echo $1 | PROMPT="" $BIN > $TEMPDIR/result
    eval $1 > $TEMPDIR/correct
    cmp --silent $TEMPDIR/result $TEMPDIR/correct
    RET=$?
    set -ue
    if [ $RET -eq 0 ] ;then
        echo "Success: $1"
    else
        echo "Failed:  $1"
        clean
        exit $RET
    fi
}

function assert_side_effect() {
    set +ue
    echo $1 | PROMPT="" $BIN
    eval $2
    eval $3
    RET=$?
    set -ue
    if [ $RET -eq 0 ] ;then
        echo "Success: $1"
    else
        echo "Failed:  $1"
        clean
        exit $RET
    fi
}

function setup() {
    echo "setup"
    stack build
    if [ -d $TEMPDIR ] ;then
        clean
    fi
    mkdir $TEMPDIR
}

function clean() {
    echo "clean"
    rm -rf $TEMPDIR
}

echo "starting test"
setup
assert "ls src"
assert " ls  src "
assert "ls | grep R"
assert "ls|grep R"
assert_side_effect "ls > $TEMPDIR/hoge" "ls > $TEMPDIR/fuga" "cmp --silent $TEMPDIR/hoge $TEMPDIR/fuga"
assert "grep H < LICENSE"
assert "ls && echo fuga"
assert "ls&&echo fuga"
assert "ls || echo hoge"
assert "ls||echo hoge"
assert "ls; echo hoge"
assert "ls;echo hoge"
assert "ls;"
assert_side_effect "ls > $TEMPDIR/hoge" "ls > $TEMPDIR/fuga" "cmp --silent $TEMPDIR/hoge $TEMPDIR/fuga"
assert_side_effect "ls | grep R > $TEMPDIR/hoge" "ls | grep R > $TEMPDIR/fuga" "cmp --silent $TEMPDIR/hoge $TEMPDIR/fuga"
assert_side_effect "cat notexistfile 2> $TEMPDIR/hoge" "cat notexistfile 2> $TEMPDIR/fuga" "cmp --silent $TEMPDIR/hoge $TEMPDIR/fuga"
assert "cd src; ls; cd .."
assert "rm -rf $TEMPDIR/hogedir 2> /dev/null; mkdir $TEMPDIR/hogedir; cp src/Hash/* $TEMPDIR/hogedir/; ls $TEMPDIR/hogedir"
clean
echo "All test past."