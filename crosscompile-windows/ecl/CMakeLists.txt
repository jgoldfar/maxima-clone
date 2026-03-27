# SPDX-License-Identifier: GPL-2.0-or-later

# Currently using a recent (as of 2026-03-20) GIT version.
# (not the current master, I want that reproducible...
set(ECL_MD5 "92c91aabe58bf15d360493904ed86850")
set(ECL_URL "https://ecl.common-lisp.dev/static/files/release/ecl-26.3.27.tgz")

ExternalProject_Add(ecl-native
  DEPENDS gmplib
  BUILD_IN_SOURCE 1
  URL "${ECL_URL}"
  DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
  URL_MD5 ${ECL_MD5}
  CONFIGURE_COMMAND ./configure --prefix=${CMAKE_BINARY_DIR}/ecl-native-root --enable-gmp=included
  BUILD_COMMAND make
  INSTALL_COMMAND make install
)

ExternalProject_Add(ecl
  DEPENDS gmplib ecl-native
  BUILD_IN_SOURCE 1
  URL "${ECL_URL}"
  DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
  URL_MD5 ${ECL_MD5}
  PATCH_COMMAND  rm -rf "${CMAKE_CURRENT_BINARY_DIR}/ecl-prefix/src/ecl/src/gmp"
        COMMAND  cp -a "${CMAKE_BINARY_DIR}/gmplib/gmplib-prefix/src/gmplib" "${CMAKE_CURRENT_BINARY_DIR}/ecl-prefix/src/ecl/src/gmp"
#        COMMAND  cp ${CMAKE_SOURCE_DIR}/ecl/strm_os.d ${CMAKE_BINARY_DIR}/ecl/ecl-prefix/src/ecl/src/c/streams/strm_os.d
        COMMAND  cp ${CMAKE_SOURCE_DIR}/ecl/asdf.lisp ${CMAKE_BINARY_DIR}/ecl/ecl-prefix/src/ecl/contrib/asdf/asdf.lisp
  CONFIGURE_COMMAND ECL_TO_RUN=${CMAKE_BINARY_DIR}/ecl-native-root/bin/ecl CFLAGS=-m64 ./configure --build=x86_64-pc-linux-gnu --host=x86_64-w64-mingw32 --prefix=${CMAKE_BINARY_DIR}/ecl-cross-root --with-cross-config=${CMAKE_BINARY_DIR}/ecl/ecl-prefix/src/ecl/src/util/x86_64-w64-mingw32.cross_config --enable-gmp=included --with-cmp=yes --with-bytecmp=no #--with-asdf=no
  BUILD_COMMAND make
  INSTALL_COMMAND make install
          COMMAND ${CMAKE_COMMAND} -E copy "${MINGW_LIBSTDCPP}" "${CMAKE_BINARY_DIR}/ecl-cross-root"
          COMMAND ${CMAKE_COMMAND} -E copy "${MINGW_LIBGCC}" "${CMAKE_BINARY_DIR}/ecl-cross-root"
          COMMAND ${CMAKE_COMMAND} -E copy "${MINGW_LIBWINPTHREAD}" "${CMAKE_BINARY_DIR}/ecl-cross-root"
)
install(DIRECTORY ${CMAKE_BINARY_DIR}/ecl-cross-root/ DESTINATION bin/ecl COMPONENT ECL)
