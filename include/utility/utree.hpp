/*==============================================================================
    Copyright (c) 2001-2011 Joel de Guzman
    Copyright (c) 2010      Bryce Lelbach

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
==============================================================================*/

#ifndef UTREE_UTILITY_H
#define UTREE_UTILITY_H

#include <mapnik/color.hpp>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wunused-local-typedef"
#pragma GCC diagnostic ignored "-Wshadow"
#pragma GCC diagnostic ignored "-Wsign-conversion"
#pragma GCC diagnostic ignored "-Wconversion"
#include <boost/lexical_cast.hpp>
#include <boost/spirit/include/support_utree.hpp>
#pragma GCC diagnostic pop

namespace carto {

namespace spirit = boost::spirit;
using spirit::utree;

struct utree_to_string {
    typedef std::string result_type;

    template<class T>
    std::string operator() (T val){
        return boost::lexical_cast<std::string>(val);
    }
    
    template<typename Iterator>
    std::string operator() (boost::iterator_range<Iterator> const& range){
        BOOST_ASSERT(false);
        
        return std::string();
    }

    std::string operator() (spirit::utf8_string_range_type const& str);
    std::string operator() (spirit::utf8_symbol_range_type const& str);
    std::string operator() (utree::invalid_type ut);
    std::string operator() (utree::nil_type ut);
    std::string operator() (spirit::binary_range_type const& str);
    std::string operator() (spirit::any_ptr const& p);
    std::string operator() (spirit::function_base const& pf);
};

template<class T>
T as(utree& ut) {
    return ut.get<T>();
}

template<class T>
T as(utree const& ut) {
    return ut.get<T>();
}

template<>
std::string as<std::string>(utree const& ut);

template<>
mapnik::color as<mapnik::color>(utree const& ut);

}

#endif
