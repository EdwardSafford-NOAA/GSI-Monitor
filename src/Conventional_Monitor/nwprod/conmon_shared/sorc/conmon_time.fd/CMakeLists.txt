if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -assume byterecl -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4")
endif()

list(APPEND SRCS
  convinfo2.f90
  mainconv_time.f90
  process_time_data.f90
  stas2ctl.f90
  stas_time.f90
  stas_time_gps.f90
)

add_executable(conmon_time.x ${SRCS})

target_compile_definitions(conmon_time.x PRIVATE "_REAL8_")

# Ensure shared modules are built first and visible for USE resolution.
add_dependencies(conmon_time.x conmon_shared_obj)
target_include_directories(conmon_time.x PRIVATE
  $<TARGET_PROPERTY:conmon_shared_obj,Fortran_MODULE_DIRECTORY>
)

target_link_libraries(conmon_time.x PRIVATE conmon_shared)

# Install executable targets
install(TARGETS conmon_time.x RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

