/*==============================================================================
    Copyright (c) 2001-2011 Joel de Guzman
    Copyright (c) 2010      Bryce Lelbach

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
==============================================================================*/

#ifndef POSITION_ITERATOR_H
#define POSITION_ITERATOR_H

#include <sstream>

#include <boost/detail/iterator.hpp>
#include <boost/iterator/iterator_adaptor.hpp>
#include <boost/range/iterator_range.hpp>

namespace carto {

struct source_location {

    int line;
    int column;

    source_location (int l, int c)
      : line(l),
        column(c)
    { }

    source_location ()
      : line(-1),
        column(-1)
    { }

    bool valid() {
        return (line != -1) && (column != -1);
    }
    
    std::string get_string() {
        std::stringstream s;
        
        if (valid())
            s << "Line: " << line << " Col: " << column;
        else 
            s << "Unknown Position";
    
        return s.str();
    }
    
    bool operator==(source_location const& other) const {
        return other.line == line && other.column == column;
    }
    
    
};


template <typename Iterator, int TabLength = 2>
class position_iterator
  : public boost::iterator_adaptor< position_iterator<Iterator>, 
                                    Iterator, 
                                    boost::use_default,
                                    boost::forward_traversal_tag >
{
public:
    position_iterator()
      : position_iterator::iterator_adaptor_(),
        loc(1, 0), 
        prev(0)
    { }

    explicit position_iterator (Iterator base)
      : position_iterator::iterator_adaptor_(base),
        loc(1, 0), 
        prev(0)
    { }

    int& line() {
        return loc.line;
    }

    int const& line() const {
        return loc.line;
    }

    int& column() {
        return loc.column;
    }

    int const& column() const {
        return loc.column;
    }

    source_location& location() {
        return loc;
    }

    source_location const& location() const {
        return loc;
    }

private:
    friend class boost::iterator_core_access;

    void increment() {
        
        using boost::detail::iterator_traits;

        typename iterator_traits<Iterator>::reference ref = *(this->base());

        switch (ref) {
            case '\r':
                if (prev != '\n') {
                    ++loc.line;
                    loc.column = 0;
                }
                break;
            case '\n':
                if (prev != '\r') {
                    ++loc.line;
                    loc.column = 0;
                }
                break;
            case '\t':
                loc.column += TabLength;
                break;
            default:
                ++loc.column;
                break;
        }

        prev = ref;
        ++this->base_reference();
    }

    source_location loc;
    typename boost::detail::iterator_traits<Iterator>::value_type prev;
};

template<class Iterator>
inline source_location get_location (Iterator const& i) {
    source_location loc(-1, -1);
    return loc;
}

template<class Iterator>
inline source_location get_location (position_iterator<Iterator> const& i) {
    return i.location();
}

}

#endif
