# SPDX-License-Identifier: GPL-2.0-or-later
set(GMPLIB_VERSION 6.3.0)
set(GMPLIB_MD5 "956dc04e864001a9c22429f761f2c283")
set(GMPLIB_URL "https://gmplib.org/download/gmp/gmp-${GMPLIB_VERSION}.tar.xz")

externalproject_add(gmplib
  URL "${GMPLIB_URL}"
  DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
  URL_MD5 ${GMPLIB_MD5}
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ""
)
