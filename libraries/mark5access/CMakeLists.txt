cmake_minimum_required(VERSION 3.20)
project(mark5access VERSION 1.7.1 LANGUAGES C)
# add author and email
set(PROJECT_AUTHOR "Walter Brisken")
set(PROJECT_EMAIL "wbrisken@nrao.edu")
set(PROJECT_DESCRIPTION "Mark5 access library")
set(PROJECT_URL "https://github.com/difx/difx/libraries/mark5access")
set(PROJECT_LICENSE_FILE_URL "https://github.com/difx/mark5access/blob/main/LICENSE")

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)
include(TestBigEndian)
test_big_endian(WORDS_BIGENDIAN)

find_library(MATH_LIBRARY m)

find_package(PkgConfig)
pkg_check_modules(FFTW3 IMPORTED_TARGET fftw3)

# Here we check if the codifio library is available
if(NOT TARGET DiFX::codifio)
  find_package(Codifio QUIET)
endif()
if(TARGET DiFX::codifio)
  set(HAVE_CODIFIO 1)
endif()

# Here we check if the mark6sg library is available
if(NOT TARGET DiFX::mark6sg)
  find_package(Mark6sg QUIET)
endif()
if(TARGET DiFX::mark6sg)
  set(HAVE_MARK6SG 1)
endif()

configure_file(config.h.cmake ${CMAKE_CURRENT_BINARY_DIR}/config.h)

set(_m5a_sources
  mark5access/mark5_stream.c
  mark5access/mark5_stream_file.c
  mark5access/mark5_stream_memory.c
  mark5access/mark5_stream_unpacker.c
  mark5access/mark5_format_vlba.c
  mark5access/mark5_format_vlba_nomod.c
  mark5access/mark5_format_mark4.c
  mark5access/mark5_format_mark5b.c
  mark5access/mark5_format_kvn5b.c
  mark5access/mark5_format_d2k.c
  mark5access/format_vdif.c
  mark5access/blanker_none.c
  mark5access/blanker_mark5.c
  mark5access/mark5bfix.c
  mark5access/mark5bfile.c
)

if(TARGET DiFX::codifio)
  list(APPEND _m5a_sources mark5access/format_codif.c)
else()
  list(APPEND _m5a_sources mark5access/format_codif_stubs.c)
endif()

if(TARGET DiFX::mark6sg)
  list(APPEND _m5a_sources mark5access/mark6gather_mark5b.c)
endif()

add_library(mark5access ${_m5a_sources})
target_include_directories(mark5access
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/mark5access>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/mark5access>
  PRIVATE
    ${CMAKE_CURRENT_BINARY_DIR}
)
target_link_libraries(mark5access PRIVATE $<$<BOOL:${MATH_LIBRARY}>:${MATH_LIBRARY}>)
if(TARGET DiFX::codifio)
  target_link_libraries(mark5access PUBLIC DiFX::codifio)
  target_compile_definitions(mark5access PRIVATE HAVE_CODIFIO=1)
endif()
if(TARGET DiFX::mark6sg)
  target_link_libraries(mark5access PUBLIC DiFX::mark6sg DiFX::mark6gather)
  target_compile_definitions(mark5access PRIVATE HAVE_MARK6SG=1)
endif()
set_target_properties(mark5access PROPERTIES
  VERSION   ${PROJECT_VERSION}
  SOVERSION 0
  PUBLIC_HEADER "mark5access/mark5_stream.h;mark5access/mark5bfix.h;mark5access/mark5bfile.h"
)
add_library(DiFX::mark5access ALIAS mark5access)

# build bbsum 
if(DIFX_BUILD_UTILS OR PROJECT_IS_TOP_LEVEL)
  add_executable(bbsum bbsum/bbsum.c)
  target_link_libraries(bbsum PRIVATE mark5access)
  install(TARGETS bbsum RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
endif()

# build examples / tools
if(DIFX_BUILD_UTILS OR PROJECT_IS_TOP_LEVEL)
  set(_m5a_examples
    directory2filelist fixmark5b m5bstate m5bsum m5d m5fold
    m5timeseries m5tsys m5test m5time m5slice m5findformats
    test5b test_mark5_stream test_unpacker)
  foreach(_ex IN LISTS _m5a_examples)
    add_executable(${_ex} examples/${_ex}.c)
    target_include_directories(${_ex} PRIVATE ${CMAKE_CURRENT_BINARY_DIR})
    target_link_libraries(${_ex} PRIVATE mark5access
      $<$<BOOL:${MATH_LIBRARY}>:${MATH_LIBRARY}>)
  endforeach()

  if(FFTW3_FOUND)
    set(_fftw_examples m5pcal m5spec m5fb m5subband zerocorr)
    foreach(_ex IN LISTS _fftw_examples)
      add_executable(${_ex} examples/${_ex}.c)
      target_include_directories(${_ex} PRIVATE ${CMAKE_CURRENT_BINARY_DIR})
      target_link_libraries(${_ex} PRIVATE mark5access PkgConfig::FFTW3
        $<$<BOOL:${MATH_LIBRARY}>:${MATH_LIBRARY}>)
    endforeach()
    pkg_check_modules(FFTW3F IMPORTED_TARGET fftw3f)
    if(FFTW3F_FOUND)
      target_link_libraries(m5subband PRIVATE PkgConfig::FFTW3F)
    endif()
    list(APPEND _m5a_examples ${_fftw_examples})
  endif()

  install(TARGETS ${_m5a_examples} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

  install(PROGRAMS examples/zerocorr_makeconfig.py
    DESTINATION ${CMAKE_INSTALL_BINDIR}
    RENAME zerocorr_makeconfig)
endif()

# Install the convenience top-level header (mark5access.h -> <prefix>/include/)
install(FILES mark5access.h DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

# Install the library and targets
install(TARGETS mark5access
  EXPORT mark5access-targets
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/mark5access
)
install(EXPORT mark5access-targets
  FILE Mark5accessTargets.cmake
  NAMESPACE DiFX::
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/Mark5access
)

configure_package_config_file(
  cmake/Mark5accessConfig.cmake.in
  ${CMAKE_CURRENT_BINARY_DIR}/Mark5accessConfig.cmake
  INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/Mark5access
)
write_basic_package_version_file(
  ${CMAKE_CURRENT_BINARY_DIR}/Mark5accessConfigVersion.cmake
  COMPATIBILITY SameMajorVersion
)
install(FILES
  ${CMAKE_CURRENT_BINARY_DIR}/Mark5accessConfig.cmake
  ${CMAKE_CURRENT_BINARY_DIR}/Mark5accessConfigVersion.cmake
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/Mark5access
)

# pkg-config
set(prefix "${CMAKE_INSTALL_PREFIX}")
set(exec_prefix "\${prefix}")
set(libdir "\${prefix}/${CMAKE_INSTALL_LIBDIR}")
set(includedir "\${prefix}/${CMAKE_INSTALL_INCLUDEDIR}")
set(PACKAGE_VERSION "${PROJECT_VERSION}")
set(MARK6SG_LIBS "")
set(CODIFIO_LIBS "")
if(TARGET DiFX::mark6sg)
  set(MARK6SG_LIBS "-lmark6sg -lmark6gather")
endif()
if(TARGET DiFX::codifio)
  set(CODIFIO_LIBS "-lcodifio")
endif()
configure_file(mark5access.pc.in ${CMAKE_CURRENT_BINARY_DIR}/mark5access.pc @ONLY)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/mark5access.pc
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig)
