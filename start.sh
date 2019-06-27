#!/bin/sh
cd `dirname $0`
rm ./var/* -rf


# +A 8 \
exec erl \
  +P 10240000 \
  +K true \
  -name meetup@10.140.2.17 \
  +zdbbl 8192 \
  -pa $PWD/_build/default/lib/*/ebin \
  -boot start_sasl -s reloader -s meetup -setcookie XEXIWPUHUJTYKXFMMTXE
