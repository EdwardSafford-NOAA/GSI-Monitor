if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -assume byterecl -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4")
endif()

add_library(conmon_shared STATIC
  conmon_read_diag.F90
  data_mod.f90
  generic_list.f90
  kinds.F90
)

target_compile_definitions(conmon_shared PUBLIC "_REAL8_")
target_link_libraries(conmon_shared PUBLIC NetCDF::NetCDF_Fortran)
target_link_libraries(conmon_shared PUBLIC ncdiag::ncdiag_serial)
target_link_libraries(conmon_shared PUBLIC w3emc::w3emc_4)

install(TARGETS conmon_shared ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})
