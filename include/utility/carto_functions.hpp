#ifndef CARTO_FUNCTIONS_H
#define CARTO_FUNCTIONS_H

#include <mapnik/color.hpp>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wunused-local-typedef"
#pragma GCC diagnostic ignored "-Wshadow"
#pragma GCC diagnostic ignored "-Wsign-conversion"
#pragma GCC diagnostic ignored "-Wconversion"
#include <boost/spirit/include/support_utree.hpp>
#pragma GCC diagnostic pop

#include <utility/utree.hpp>
#include <utility/round.hpp>

namespace carto {

inline double clamp(double val);

struct hsl 
{    
    double h,s,l,a;
    unsigned tag;
    
    hsl(utree const& rgb);

    double hue(double h, double m1, double m2);

    utree to_rgb() ;
};

utree test(utree const& rgb);
utree hue(utree const& rgb);
utree saturation(utree const& rgb);
utree lightness(utree const& rgb);
utree alpha(utree const& rgb);
utree saturate(utree const& rgb, utree const& value) ;
utree desaturate(utree const& rgb, utree const& value);
utree lighten(utree const& rgb, utree const& value);
utree darken(utree const& rgb, utree const& value);
utree fadein(utree const& rgb, utree const& value);
utree fadeout(utree const& rgb, utree const& value);
utree spin(utree const& rgb, utree const& value);
utree mix(utree const& col1, utree const& col2, utree const& weight);
utree greyscale(utree const& rgb);

}

#endif
