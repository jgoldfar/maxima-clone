#!/bin/sh
MAXIMA=@MAXIMA@
LISP=@LISP@
OUTPUT=@OUTPUT@
PRELOAD=./mathmltest.mac

$MAXIMA  --very-quiet --lisp=$LISP -p $PRELOAD --batch-string='mathml_batch("./example.mac");' \
	 | sed '/^;\+/{d}; /^read and interpret/{d}; /loadfile: /{d}; /^[[:space:]]\*$/{d};' > $OUTPUT
