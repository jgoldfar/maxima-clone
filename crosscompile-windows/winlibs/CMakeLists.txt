# SPDX-License-Identifier: GPL-2.0-or-later
set(WINLIBS_MD5 "b6b4eb6ad019ecda142bd3dc602dfd2d")
set(WINLIBS_URL "https://github.com/brechtsanders/winlibs_mingw/releases/download/15.2.0posix-13.0.0-ucrt-r6/winlibs-x86_64-posix-seh-gcc-15.2.0-mingw-w64ucrt-13.0.0-r6.7z")

externalproject_add(winlibs
  URL "${WINLIBS_URL}"
  DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
  URL_MD5 ${WINLIBS_MD5}
  CONFIGURE_COMMAND ""
  BUILD_COMMAND  ${CMAKE_COMMAND} -E copy_directory "${CMAKE_BINARY_DIR}/winlibs/winlibs-prefix/src/winlibs" "${CMAKE_BINARY_DIR}/mingw"
        COMMAND  ${CMAKE_COMMAND} -E copy "${CMAKE_BINARY_DIR}/mingw/bin/gcc.exe" "${CMAKE_BINARY_DIR}/mingw/bin/cc.exe"
  INSTALL_COMMAND ""
)
