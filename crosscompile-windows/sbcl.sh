#!/bin/sh
# SPDX-License-Identifier: GPL-2.0-or-later

# recreate SBCL directory structure from the extracted installer

mkdir -p contrib
# Move files File_obj_sbcl.home_contrib_* in the directory "contrib" and remove the "File_obj_sbcl.home_contrib_"-prefix:
for i in File_obj_sbcl.home_contrib_* ; do
    mv "$i" contrib/$(echo "$i" | sed s/File_obj_sbcl.home_contrib_//)
done
# Rename files in contrib directory - every but the last "." should be a "-".
# (e.g. contrib/sb-bsd.sockets.fasl ==> contrib/sb-bsd-sockets.fasl
for i in contrib/sb.* ; do
    mv "$i" $(echo "$i" | sed 's/\./-/g' | rev | sed  s/-/\./ | rev)
done

# Crosscompiling Maxima (32 bit) on Ubuntu 24.04 causes memory problems (did work on 22.04).
# Set the large-address-aware flag (for 32bit) for SBCL.
# output the checksum of the binary before and after (should be unmodified for 64bit)
md5sum sbcl.exe
genpeimg -p +l sbcl.exe
md5sum sbcl.exe

