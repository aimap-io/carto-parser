cmake_minimum_required(VERSION 3.3)
project(carto_parser)

set(Boost_DEBUG ON)
set(Boost_USE_STATIC_LIBS   ON)
set(Boost_USE_MULTITHREADED ON)
find_package(Boost 1.59.0 COMPONENTS system filesystem REQUIRED)

INCLUDE_DIRECTORIES(
        /usr/local/include
        /usr/local/opt/icu4c/include
        ${PROJECT_SOURCE_DIR}/include
)

INCLUDE_DIRECTORIES(${BOOST_INCLUDE_DIRS})
LINK_DIRECTORIES(
        /usr/local/lib
        /usr/local/opt/icu4c/lib
        ${BOOST_LIB_DIRS}
)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -I/usr/local/include -I/usr/local/include/mapnik/agg -I/usr/local/Cellar/gdal/1.11.3/include -I/usr/local/Cellar/postgresql/9.4.5_2/include -I/usr/local/opt/libtiff/include -I/usr/local/opt/webp/include -I/usr/local/opt/libpng/include -I/usr/local/opt/proj/include -I/usr/local/opt/jpeg/include -I/usr/local/Cellar/freetype/2.6_1/include/freetype2 -I/usr/local/opt/harfbuzz/include -I/usr/local/opt/icu4c/include -I/usr/local/opt/boost/include -I/usr/include -I/usr/local/Cellar/libpng/1.6.19/include/libpng16 -I/usr/local/Cellar/glib/2.46.2/include/glib-2.0 -I/usr/local/Cellar/glib/2.46.2/lib/glib-2.0/include -I/usr/local/opt/gettext/include -I/usr/local/Cellar/fontconfig/2.11.1/include -I/usr/local/Cellar/pixman/0.32.8/include/pixman-1 -I/usr/local/Cellar/cairo/1.14.4/include/cairo -DSHAPE_MEMORY_MAPPED_FILE -DMAPNIK_HAS_DLCFN -DBIGINT -DHAVE_JPEG -DMAPNIK_USE_PROJ4 -DHAVE_PNG -DHAVE_WEBP -DHAVE_TIFF -DDARWIN -DMAPNIK_THREADSAFE -DBOOST_PHOENIX_NO_PREDEFINED_TERMINALS=1 -DBOOST_SPIRIT_USE_PHOENIX_V3=1 -DNDEBUG -DHAVE_CAIRO -DGRID_RENDERER -std=c++11 -stdlib=libc++ -DBOOST_EXCEPTION_DISABLE -Wall -ftemplate-depth-300 -Wsign-compare -Wshadow -Wno-unknown-pragmas -Wno-unsequenced -g")

set(SOURCE_FILES
    include/generate/generate_filter.hpp
    include/generate/generate_json.hpp
    include/generate/generate_json_dot.hpp
    include/generate/generate_mss_dot.hpp
    include/parse/annotator.hpp
    include/parse/carto_grammar.hpp
    include/parse/error_handler.hpp
    include/parse/expression_grammar.hpp
    include/parse/filter_grammar.hpp
    include/parse/json_grammar.hpp
    include/parse/node_types.hpp
    include/parse/parse_tree.hpp
    include/utility/carto_error.hpp
    include/utility/carto_functions.hpp
    include/utility/color.hpp
    include/utility/environment.hpp
    include/utility/position_iterator.hpp
    include/utility/round.hpp
    include/utility/source_location.hpp
    include/utility/utree.hpp
    include/utility/version.hpp
    include/exception.hpp
    include/expression_eval.hpp
    include/mml_parser.hpp
    include/mss_parser.hpp
    src/utility/carto_error.cpp
    src/utility/carto_functions.cpp
    src/utility/color.cpp
    src/utility/environment.cpp
    src/utility/round.cpp
    src/utility/source_location.cpp
    src/utility/utree.cpp
    src/utility/version.cpp
    src/annotator.cpp
    src/exception.cpp
    src/expression_eval.cpp
    src/generate_filter.cpp
    src/mml_parser.cpp
    src/mss_parser.cpp
)

add_executable(carto src/main.cpp ${SOURCE_FILES})
target_link_libraries(carto boost_system-mt boost_program_options-mt boost_regex-mt boost_filesystem-mt mapnik mapnik-wkt mapnik-json cairo png proj tiff webp harfbuzz jpeg icuuc freetype z)

add_executable(render tools/render.cpp ${SOURCE_FILES})
target_link_libraries(render boost_system-mt boost_program_options-mt boost_regex-mt boost_filesystem-mt mapnik mapnik-wkt mapnik-json cairo png proj tiff webp harfbuzz jpeg icuuc freetype z)

add_executable(expression_test tools/expression_test.cpp ${SOURCE_FILES})
target_link_libraries(expression_test boost_system-mt boost_program_options-mt boost_regex-mt boost_filesystem-mt mapnik mapnik-wkt mapnik-json cairo png proj tiff webp harfbuzz jpeg icuuc freetype z)
