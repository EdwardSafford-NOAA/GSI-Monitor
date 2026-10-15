if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict -assume byterecl -convert big_endian")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian -ffree-line-length-0 -ffast-math -fno-second-underscore -frecord-marker=4")
endif()

set(CONMON_SHARED_SRCS
  conmon_read_diag.F90
  data_mod.f90
  generic_list.f90
  kinds.F90
)

# Compile shared module sources once; reuse objects in archive targets.
add_library(conmon_shared_obj OBJECT ${CONMON_SHARED_SRCS})

# Ensure generated .mod files are emitted in one location and exported
# to dependent targets that USE these modules.
set_target_properties(conmon_shared_obj PROPERTIES
  Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules
)

target_include_directories(conmon_shared_obj
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/modules>
)

target_compile_definitions(conmon_shared_obj PUBLIC "_REAL8_")
target_link_libraries(conmon_shared_obj PUBLIC NetCDF::NetCDF_Fortran)
target_link_libraries(conmon_shared_obj PUBLIC ncdiag::ncdiag_serial)
target_link_libraries(conmon_shared_obj PUBLIC w3emc::w3emc_4)

add_library(conmon_shared STATIC $<TARGET_OBJECTS:conmon_shared_obj>)
target_link_libraries(conmon_shared
  PUBLIC
    conmon_shared_obj
    NetCDF::NetCDF_Fortran
    ncdiag::ncdiag_serial
    w3emc::w3emc_4
)

install(TARGETS conmon_shared ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR})
