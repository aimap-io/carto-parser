/*==============================================================================
    Copyright (c) 2010 Colin Rundel

    Distributed under the Boost Software License, Version 1.0. (See accompanying
    file BOOST_LICENSE_1_0.rst or copy at http://www.boost.org/LICENSE_1_0.txt)
==============================================================================*/

#include <mss_parser.hpp>

#include <fstream>

#include <mapnik/color_factory.hpp>
#include <mapnik/font_engine_freetype.hpp>
#include <mapnik/font_set.hpp>
#include <mapnik/symbolizer.hpp>
#include <mapnik/feature_type_style.hpp>
#include <mapnik/expression_string.hpp>
#include <mapnik/expression.hpp>
#include <mapnik/version.hpp>
#include <mapnik/rule.hpp>
#include <mapnik/image_scaling.hpp>
#include <mapnik/parse_transform.hpp>
#include <mapnik/text/placements/list.hpp>
#include <mapnik/text/placements/simple.hpp>

#include <expression_eval.hpp>
#include <generate/generate_filter.hpp>
#include <utility/utree.hpp>
#include <utility/environment.hpp>
#include <utility/version.hpp>
#include <utility/round.hpp>
#include <utility/carto_error.hpp>

#include <parse/carto_grammar.hpp>
#include <parse/parse_tree.hpp>
#include <parse/json_grammar.hpp>

namespace carto {

template<typename symbolizer> inline
static symbolizer&& init_symbolizer(symbolizer&& sym) {
    return std::move(sym);
}

template<> inline
mapnik::text_symbolizer&& init_symbolizer<mapnik::text_symbolizer>(mapnik::text_symbolizer&& sym)
{
    auto placement = mapnik::text_placements_ptr(new mapnik::text_placements_list);
    mapnik::put(sym,
                mapnik::keys::text_placements_,
                placement);
    //return mapnik::text_symbolizer(boost::make_shared<mapnik::expr_node>(true),
    //                               "<no default>", 0,
    //                               mapnik::color(0,0,0) );

    //return mapnik::text_symbolizer(mapnik::expression_ptr(), "<no default>", 0,
    //                               mapnik::color(0,0,0) );

    return std::move(sym);
}

template<> inline
mapnik::shield_symbolizer&& init_symbolizer<mapnik::shield_symbolizer>(mapnik::shield_symbolizer&& sym)
{
    return std::move(sym);
    //return mapnik::shield_symbolizer(mapnik::expression_ptr(), "<no default>", 0,
    //                                 mapnik::color(0,0,0), mapnik::path_expression_ptr());
}

template<> inline
mapnik::polygon_pattern_symbolizer&& init_symbolizer<mapnik::polygon_pattern_symbolizer>(mapnik::polygon_pattern_symbolizer&& sym)
{
    return std::move(sym);
    //return mapnik::polygon_pattern_symbolizer(mapnik::parse_path(""));
}

template<> inline
mapnik::line_pattern_symbolizer&& init_symbolizer<mapnik::line_pattern_symbolizer>(mapnik::line_pattern_symbolizer&& sym)
{
    //return mapnik::line_pattern_symbolizer(mapnik::parse_path(""));
    return std::move(sym);
}

template<class symbolizer> inline
static symbolizer& to_symbolizer(mapnik::rule& rule)
{
    for (auto sym : rule) {
        if (sym.is<symbolizer>()) {
            return sym.get<symbolizer>();
        }
    }

    rule.append(init_symbolizer(symbolizer()));
    return (--rule.end())->get<symbolizer>();
}

mapnik::transform_type mss_parser::create_transform(std::string const& str, utree const& node)
{
    mapnik::transform_type trans( mapnik::parse_transform(str) );

    if (!trans)
    {
        std::stringstream stream;
        stream << "Could not parse transform from '" << str << "', expected transform attribute";

        carto_error err(stream.str(), get_location(node));
        if (strict) throw err;
        else        warn(err);
    }

    return trans;
}

void mss_parser::key_error(std::string const& key, utree const& node) {

    std::string str = "Unknown variable: @" + key;

    carto_error err(str, get_location(node));
    if (strict) throw err;
    else        warn(err);
}

mss_parser::mss_parser(parse_tree const& pt, std::string const& path_, bool strict_)
  : tree(pt),
    strict(strict_),
    path(path_),
    expr_grammar()
{}
  
mss_parser::mss_parser(std::string const& in, std::string const& path_, bool strict_)
  : strict(strict_),
    path(path_),
    expr_grammar()
{ 
    typedef position_iterator<std::string::const_iterator> iter;
    tree = build_parse_tree< carto_parser<iter> >(in, path);    
}

mss_parser::mss_parser(std::string const& filename, bool strict_)
  : strict(strict_),
    path(filename),
    expr_grammar()
{ 

    std::ifstream file(filename.c_str(), std::ios_base::in);

    if (!file)
        throw carto_error(std::string("Cannot open input file: ")+filename);
    
    std::string in;
    file.unsetf(std::ios::skipws);
    copy(std::istream_iterator<char>(file),
         std::istream_iterator<char>(),
         std::back_inserter(in));

    typedef position_iterator<std::string::const_iterator> iter;
    tree = build_parse_tree< carto_parser<iter> >(in, path);    
}

parse_tree mss_parser::get_parse_tree()
{
    return tree;
}

std::string mss_parser::get_path()
{
    return path;
}

int mss_parser::get_node_type(utree const& ut)
{   
    return( tree.annotations(ut.tag()).second );
}

source_location mss_parser::get_location(utree const& ut)
{    
    return tree.annotations()[ut.tag()].first;
}

std::string const& mss_parser::get_fontset_name(std::size_t hash) 
{
    typedef boost::unordered_map<std::size_t, std::string> umap;

    umap::const_iterator it = fontset_names.find(hash);

    if (it == fontset_names.end()) {
        fontset_names[hash] = "fontset-"+boost::lexical_cast<std::string>(fontset_names.size()-1);
        return fontset_names[hash];
    } else {
        return it->second;
    }
}

void mss_parser::parse(mapnik::Map& map, style_env& env)
{
    try {
        parse_stylesheet(map, env);
    } catch(carto_error& e) {
        e.set_filename(path);
        throw e;
    }
}

void mss_parser::parse_stylesheet(mapnik::Map& map, style_env& env)
{
    using spirit::utree_type;
    
    utree const& root_node = tree.ast();
    
    typedef utree::const_iterator iter;
    iter it = root_node.begin(),
        end = root_node.end();
    
    for (; it != end; ++it) {
        switch((carto_node_type) get_node_type(*it)) {
            case CARTO_VARIABLE:
                parse_variable(*it,env);
                break;
            case CARTO_MAP_STYLE:
                parse_map_style(map, *it, env);
                break;
            case CARTO_STYLE:
                parse_style(map, *it, env);
                break;
            case CARTO_MIXIN:
            case CARTO_COMMENT:
                break;
            default:
            {
                std::stringstream out;
                out << "Invalid stylesheet node type: " << get_node_type(*it);
                
                throw carto_error(out.str(), get_location(*it));
            }
        }
      
     }
}

void mss_parser::parse_style(mapnik::Map& map,
                             utree const& node,
                             style_env const& parent_env,
                             mapnik::rule const& parent_rule,
                             std::string const& parent_name)
{
    
    BOOST_ASSERT(node.size()==2);
    
    typedef utree::const_iterator iter;
    iter style_it  = node.front().begin(),
         style_end = node.front().end();
    
    for (; style_it != style_end; ++style_it) {
        
        style_env env(parent_env);
        mapnik::rule rule(parent_rule);
        
        BOOST_ASSERT(style_it->size() == 3);
        iter name_it  = (*style_it).begin(),
             name_end = (*style_it).end();
        
        utree const& uname   = *name_it; name_it++;
        utree const& uattach = *name_it; name_it++;
        utree const& ufilter = *name_it;
        
        std::string name = parent_name + as<std::string>(uname);
        
        mapnik::Map::style_iterator map_it, map_end;
        
        map_it  = map.styles().find(name);                           
        map_end = map.styles().end();
        
        if (map_it == map_end) {
            
            mapnik::feature_type_style new_style;
            new_style.set_filter_mode(mapnik::FILTER_FIRST);
            
            map.insert_style(name, new_style);
            map_it = map.styles().find(name);
        }
        
        if (uattach.size() != 0) {
            name += "::"+as<std::string>(uattach);
            
            map.insert_style(name, mapnik::feature_type_style((*map_it).second));
            map_it = map.styles().find(name);
        }
        
        if (ufilter.size() != 0) {
            BOOST_ASSERT(get_node_type(ufilter) == CARTO_FILTER);
            parse_filter(map, ufilter, env, rule);
        }
        
        iter it  = node.back().begin(),
             end = node.back().end();
    
        for (; it != end; ++it) {
            switch(get_node_type(*it)) {
                case CARTO_VARIABLE:
                    parse_variable(*it,env);
                    break;
                case CARTO_STYLE:
                    parse_style(map, *it, env, rule, name);
                    break;
                case CARTO_ATTRIBUTE:
                    parse_attribute(map, *it, env, rule);                    
                    break;
                case CARTO_MIXIN:
                case CARTO_COMMENT:
                    break;
                default:
                    std::stringstream out;
                    out << "Invalid style node type: " << get_node_type(*it);
                    
                    throw carto_error(out.str(), get_location(*it));
            }
        }
        
        //mapnik::rules& rules = style->get_rules_nonconst();
        if (rule.get_symbolizers().size() != 0) {
            //rules[pos] = rule;
            (*map_it).second.add_rule(std::move(rule));
        } else {
            //map.styles().erase(map_it);
        }
    }
}

void mss_parser::parse_filter(mapnik::Map& map, utree const& node, style_env const& env, mapnik::rule& rule)
{
    if (node.size() == 0) return;
    
    std::string cur_filter = mapnik::to_expression_string(*rule.get_filter()),
                out = (cur_filter != "true") ? "(" + cur_filter + ")" : "";
    
    utree::const_iterator it  = node.begin(),
                          end = node.end();
                          
    for (; it != end; ++it) 
    {
        filter_printer printer(*it, tree.annotations(), env, rule);
        std::string str = printer.print();
        
        if (str.empty()) continue;
        
        out += (!out.empty() ? " and " : "") + str;
    }
    
    if (!out.empty()) {
        auto expr = mapnik::parse_expression(out);
        rule.set_filter(expr);
    }
}

utree mss_parser::eval_var(utree const& node, style_env const& env) {
    std::string key = as<std::string>(node.front());
    
    utree value = env.vars.lookup(key);
    
    if (value == utree::nil_type()) {
        key_error(key, node);
    }
    
    return (get_node_type(value) == CARTO_VARIABLE) ? eval_var(value,env) : value;
}

utree mss_parser::parse_value(utree const& node, style_env const& env) 
{
    if (get_node_type(node) == CARTO_VARIABLE) {
        return eval_var(node, env); // vars can point at other vars
    } else if (get_node_type(node) == CARTO_EXPRESSION) {
        //BOOST_ASSERT(node.size()==1);
        expression exp(node.front().front(), tree.annotations(), env);
        return exp.eval();
    } else {
        if (node.size() == 1)
            return node.front();
        else 
            return node;
    }
}

void mss_parser::parse_attribute(mapnik::Map& map,
                                 utree const& node,
                                 style_env const& env,
                                 mapnik::rule& r) {
    BOOST_ASSERT(node.size() == 2);

    using parse_map = std::map<std::string, std::function<void(utree const &, mapnik::rule &)>>;
    static parse_map m{
            {"polygon-fill", [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::polygon_symbolizer>(rule),
                            mapnik::keys::fill,
                            as<mapnik::color>(value));
            }},
            {"polygon-gamma",                 [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::polygon_symbolizer>(rule),
                            mapnik::keys::gamma,
                            as<double>(value));
            }},
            {"polygon-opacity",               [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::polygon_symbolizer>(rule),
                            mapnik::keys::opacity,
                            as<double>(value));
            }},
            {"line-dasharray",                [this](utree const &value, mapnik::rule &rule) {
                BOOST_ASSERT(((value.size() - 1) % 2 == 0));

                mapnik::dash_array dash_array;
                for (auto itr = value.begin(); itr != value.end(); ++itr) {
                    auto dash = as<double>(*itr);
                    ++itr;
                    dash_array.emplace_back(dash, as<double>(*itr));
                }
                mapnik::put(to_symbolizer<mapnik::line_symbolizer>(rule),
                            mapnik::keys::stroke_dasharray,
                            dash_array);
            }},
            {"line-color",                    [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::line_symbolizer>(rule),
                            mapnik::keys::stroke,
                            as<mapnik::color>(value));
            }},
            {"line-width",                    [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::line_symbolizer>(rule),
                            mapnik::keys::stroke_width,
                            as<double>(value));
            }},
            {"line-opacity",                  [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::line_symbolizer>(rule),
                            mapnik::keys::stroke_opacity,
                            as<double>(value));
            }},
            {"line-join",                     [this](utree const &value, mapnik::rule &rule) {
                mapnik::line_join_e en;
                en.from_string(as<std::string>(value));
                mapnik::put(to_symbolizer<mapnik::line_symbolizer>(rule),
                            mapnik::keys::stroke_linejoin,
                            en);
            }},
            {"line-cap",                      [this](utree const &value, mapnik::rule &rule) {
                mapnik::line_cap_e en;
                en.from_string(as<std::string>(value));
                mapnik::put(to_symbolizer<mapnik::line_symbolizer>(rule),
                            mapnik::keys::stroke_linecap,
                            en);
            }},
            {"line-gamma",                    [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::line_symbolizer>(rule),
                            mapnik::keys::gamma,
                            as<double>(value));
            }},
            {"line-dash-offset",              [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::line_symbolizer>(rule),
                            mapnik::keys::stroke_dashoffset,
                            as<double>(value));

            }},
            {"marker-file",                   [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::markers_symbolizer>(rule),
                            mapnik::keys::file,
                            mapnik::parse_path(as<std::string>(value)));
            }},
            {"marker-opacity",                [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::markers_symbolizer>(rule),
                            mapnik::keys::opacity,
                            as<float>(value));
            }},
            {"marker-placement",              [this](utree const &value, mapnik::rule &rule) {
                mapnik::marker_placement_e en;
                en.from_string(as<std::string>(value));
                mapnik::put(to_symbolizer<mapnik::markers_symbolizer>(rule),
                            mapnik::keys::markers_placement_type,
                            en);
            }},
            {"marker-width",                  [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::markers_symbolizer>(rule),
                            mapnik::keys::width,
                            mapnik::parse_expression(as<std::string>(value)));
            }},
            {"marker-height",                 [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::markers_symbolizer>(rule),
                            mapnik::keys::height,
                            mapnik::parse_expression(as<std::string>(value)));
            }},
            {"marker-fill",                   [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::markers_symbolizer>(rule),
                            mapnik::keys::fill,
                            as<mapnik::color>(value));
            }},
            {"marker-allow-overlap",          [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::markers_symbolizer>(rule),
                            mapnik::keys::allow_overlap,
                            as<bool>(value));
            }},
            {"marker-spacing",                [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::markers_symbolizer>(rule),
                            mapnik::keys::spacing,
                            as<double>(value));
            }},
            {"marker-max-error",              [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::markers_symbolizer>(rule),
                            mapnik::keys::max_error,
                            as<double>(value));
            }},
            //{"marker-transform", [this](utree const& value, mapnik::rule& rule) {
            //    mapnik::put(to_symbolizer<mapnik::markers_symbolizer>(rule),
            //                mapnik::keys::,
            //    s->set_transform(create_transform(as<std::string>(value), value));
            //}},
            {"marker-line-color",             [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::markers_symbolizer>(rule),
                            mapnik::keys::stroke,
                            as<mapnik::color>(value));
            }},
            {"marker-line-width",             [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::markers_symbolizer>(rule),
                            mapnik::keys::stroke_width,
                            as<double>(value));
            }},
            {"marker-line-opacity",           [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::markers_symbolizer>(rule),
                            mapnik::keys::stroke_opacity,
                            as<double>(value));
            }},
            {"point-file",                    [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::point_symbolizer>(rule),
                            mapnik::keys::file,
                            mapnik::parse_path(as<std::string>(value)));
            }},
            {"point-allow-overlap",           [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::point_symbolizer>(rule),
                            mapnik::keys::allow_overlap,
                            as<bool>(value));

            }},
            {"point-ignore-placement",        [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::point_symbolizer>(rule),
                            mapnik::keys::ignore_placement,
                            as<bool>(value));

            }},
            {"point-opacity",                 [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::point_symbolizer>(rule),
                            mapnik::keys::opacity,
                            as<float>(value));

            }},
            {"point-placement",               [this](utree const &value, mapnik::rule &rule) {
                mapnik::point_placement_e en;
                en.from_string(as<std::string>(value));
                mapnik::put(to_symbolizer<mapnik::point_symbolizer>(rule),
                            mapnik::keys::point_placement_type,
                            en);
            }},
            {"point-transform",               [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::point_symbolizer>(rule),
                            mapnik::keys::geometry_transform,
                            create_transform(as<std::string>(value), value));

            }},
            {"line-pattern-file",             [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::line_pattern_symbolizer>(rule),
                            mapnik::keys::file,
                            mapnik::parse_path(as<std::string>(value)));

            }},
            {"polygon-pattern-file",          [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::polygon_pattern_symbolizer>(rule),
                            mapnik::keys::file,
                            mapnik::parse_path(as<std::string>(value)));

            }},
            {"polygon-pattern-alignment",     [this](utree const &value, mapnik::rule &rule) {
                mapnik::pattern_alignment_e aligmnet;
                aligmnet.from_string(as<std::string>(value));
                mapnik::put(to_symbolizer<mapnik::polygon_pattern_symbolizer>(rule),
                            mapnik::keys::alignment,
                            aligmnet);
            }},
            {"raster-opacity",                [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::raster_symbolizer>(rule),
                            mapnik::keys::opacity,
                            as<float>(value));

            }},
            {"raster-mode",                   [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::raster_symbolizer>(rule),
                            mapnik::keys::mode,
                            as<std::string>(value));
            }},
            {"raster-scaling",                [this](utree const &value, mapnik::rule &rule) {
                std::string str(as<std::string>(value));
                auto sm = mapnik::scaling_method_from_string(str);

                if (sm) {
                    mapnik::put(to_symbolizer<mapnik::raster_symbolizer>(rule),
                                mapnik::keys::scaling,
                                *sm);
                } else {
                    std::stringstream ss;
                    ss << "Invalid scaling method '" << str << "'";

                    carto_error err(ss.str(), get_location(value));
                    if (strict) {
                        throw err;
                    }
                    else {
                        warn(err);
                    }
                }
            }},
            {"building-fill",                 [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::building_symbolizer>(rule),
                            mapnik::keys::fill,
                as<mapnik::color>(value));
            }},
            {"building-fill-opacity",         [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::building_symbolizer>(rule),
                            mapnik::keys::opacity,
                            as<double>(value));

            }},
            {"building-height",               [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::building_symbolizer>(rule),
                            mapnik::keys::height,
                            mapnik::parse_expression(as<std::string>(value)));

            }},
            {"text-face-name",                [this](utree const &value, mapnik::rule &rule) {
                                                      /*
                                                          if (value.which() != utree::list_type) {
                                                              s->set_face_name(as<std::string>(value));
                                                          } else {

                                                              mapnik::font_set fs;
                                                              std::size_t hash = 0;

                                                              typedef utree::const_iterator iter;
                                                              iter it = value.begin(),
                                                                      end = value.end();

                                                              for( ; it!=end; ++it) {
                                                                  std::string str = as<std::string>(*it);
                                                                  boost::hash_combine(hash, str);
                                                                  fs.add_face_name(str);
                                                              }

                                                              fs.set_name( get_fontset_name(hash) );

                                                              s->set_fontset(fs);
                                                              s->set_face_name(std::string());
                                                              map.insert_fontset(fs.get_name(), fs);
                                                          }*/
                                              }},
            {"text-name",                     [this](utree const &value, mapnik::rule &rule) {
                auto placements =
                        mapnik::get<mapnik::text_placements_ptr>(
                                to_symbolizer<mapnik::text_symbolizer>(rule),
                                mapnik::keys::text_placements_);
                //placements->defaults.format_defaults.text_name =
                //        mapnik::parse_expression(as<std::string>(value)));

            }},
            {"text-size",                     [this](utree const &value, mapnik::rule &rule) {

                //s->set_text_size(round(as<double>(value)));

            }},
            {"text-ratio",                    [this](utree const &value, mapnik::rule &rule) {
                //s->set_text_ratio(round(as<double>(value)));

            }},
            {"text-wrap-width",               [this](utree const &value, mapnik::rule &rule) {
                //s->set_wrap_width(round(as<double>(value)));

            }},
            {"text-spacing",                  [this](utree const &value, mapnik::rule &rule) {
                //s->set_label_spacing(round(as<double>(value)));

            }},
            {"text-character-spacing",        [this](utree const &value, mapnik::rule &rule) {
                //s->set_character_spacing(round(as<double>(value)));

            }},
            {"text-line-spacing",             [this](utree const &value, mapnik::rule &rule) {
                //s->set_line_spacing(round(as<double>(value)));

            }},
            {"text-label-position-tolerance", [this](utree const &value, mapnik::rule &rule) {
                //s->set_label_position_tolerance(round(as<double>(value)));

            }},
            {"text-max-char-angle-delta",     [this](utree const &value, mapnik::rule &rule) {
                //s->set_max_char_angle_delta(as<double>(value));

            }},
            {"text-fill",                     [this](utree const &value, mapnik::rule &rule) {
                //s->set_fill(as<mapnik::color>(value));

            }},
            {"text-opacity",                  [this](utree const &value, mapnik::rule &rule) {
                //s->set_text_opacity(as<double>(value));

            }},
            {"text-halo-fill",                [this](utree const &value, mapnik::rule &rule) {
                //s->set_halo_fill(as<mapnik::color>(value));

            }},
            {"text-halo-radius",              [this](utree const &value, mapnik::rule &rule) {
                //s->set_halo_radius(as<double>(value));
                //} else if (key == "text-dx") {
                //    double x = as<double>(value);
                //    double y = s->get_displacement().second;
                //    s->set_displacement(x,y);
                //} else if (key == "text-dy") {
                //    double x = s->get_displacement().first;
                //    double y = as<double>(value);
                //    s->set_displacement(x,y);
            }},
            {"text-vertical-alignment",       [this](utree const &value, mapnik::rule &rule) {
                mapnik::vertical_alignment_e en;
                en.from_string(as<std::string>(value));
                //s->set_vertical_alignment(en);
            }},
            {"text-avoid-edges",              [this](utree const &value, mapnik::rule &rule) {
                //s->set_avoid_edges(as<bool>(value));

            }},
            {"text-min-distance",             [this](utree const &value, mapnik::rule &rule) {
                //s->set_minimum_distance(as<double>(value));

            }},
            {"text-min-padding",              [this](utree const &value, mapnik::rule &rule) {
                //s->set_minimum_padding(as<double>(value));

            }},
            {"text-allow-overlap",            [this](utree const &value, mapnik::rule &rule) {
                //s->set_allow_overlap(as<bool>(value));

            }},
            {"text-placement",                [this](utree const &value, mapnik::rule &rule) {
                mapnik::label_placement_e en;
                en.from_string(as<std::string>(value));
                //s->set_label_placement(en);
            }},
            {"text-placement-type",           [this](utree const &value, mapnik::rule &rule) {

            }},
            {"text-placements",               [this](utree const &value, mapnik::rule &rule) {

            }},
            {"text-transform",                [this](utree const &value, mapnik::rule &rule) {
                mapnik::text_transform_e en;
                en.from_string(as<std::string>(value));
                //s->set_text_transform(en);
            }},
            {"shield-name",                   [this](utree const &value, mapnik::rule &rule) {
                //mapnik::put(to_symbolizer<mapnik::shield_symbolizer>(rule),
                //            mapnik::keys
                //s->set_name(mapnik::parse_expression(as<std::string>(value)));

            }},
            {"shield-face-name",              [this](utree const &value, mapnik::rule &rule) {
                //s->set_face_name(as<std::string>(value));

            }},
            {"shield-size",                   [this](utree const &value, mapnik::rule &rule) {
                //s->set_text_size(round(as<double>(value)));

            }},
            {"shield-spacing",                [this](utree const &value, mapnik::rule &rule) {
                //s->set_label_spacing(round(as<double>(value)));

            }},
            {"shield-character-spacing",      [this](utree const &value, mapnik::rule &rule) {
                //s->set_character_spacing(round(as<double>(value)));

            }},
            {"shield-line-spacing",           [this](utree const &value, mapnik::rule &rule) {
                //s->set_line_spacing(round(as<double>(value)));

            }},
            {"shield-fill",                   [this](utree const &value, mapnik::rule &rule) {
                mapnik::put(to_symbolizer<mapnik::building_symbolizer>(rule),
                            mapnik::keys::fill,
                            as<mapnik::color>(value));
                //} else if (key == "shield-text-dx") {
                //    double x = as<double>(value);
                //    double y = s->get_displacement().second;
                //    s->set_displacement(x,y);
                //} else if (key == "shield-text-dy") {
                //    double x = s->get_displacement().first;
                //    double y = as<double>(value);
                //    s->set_displacement(x,y);
            }},
            {"shield-dx",                     [this](utree const &value, mapnik::rule &rule) {
                //double x = as<double>(value);
                //double y = s->get_shield_displacement().second;
                //s->set_shield_displacement(x,y);
            }},
            {"shield-dy",                     [this](utree const &value, mapnik::rule &rule) {
                //double x = s->get_shield_displacement().first;
                //double y = as<double>(value);
                //s->set_shield_displacement(x,y);
            }},
            {"shield-min-distance",           [this](utree const &value, mapnik::rule &rule) {
                //s->set_minimum_distance(as<double>(value));

            }},
            {"shield-placement",              [this](utree const &value, mapnik::rule &rule) {
                mapnik::label_placement_e en;
                en.from_string(as<std::string>(value));
                //s->set_label_placement(en);
            }}
    };

    std::string key = as<std::string>(node.front());
    auto itr = m.find(key);
    if (itr != m.end()) {
        itr->second(parse_value(node.back(), env), r);
    }
    else {
        //key_error(key, node);
    }
}

void mss_parser::parse_variable(utree const& node, style_env& env)
{
    std::string name = as<std::string>(node.front());
    utree val = parse_value(node.back(), env);
    env.vars.define(name, val);
}

void mss_parser::parse_map_style(mapnik::Map& map,
                                 utree const& node,
                                 style_env& env)
{
    typedef utree::const_iterator iter;
    iter it = node.begin(),
        end = node.end();
    
    mapnik::parameters params;
    bool relative_to_xml = true;
    
    for (; it != end; ++it) {
        
        BOOST_ASSERT((*it).size()==2);
        
        if (get_node_type(*it) == CARTO_VARIABLE) {
            parse_variable(*it,env);
            break;
        }
        
        std::string key = as<std::string>((*it).front());
        utree const& value = parse_value((*it).back(),env);
        
        std::string base = "";

        if (key == "srs") {
            map.set_srs(as<std::string>(value));
        } else if (key == "background-color") {
            BOOST_ASSERT((carto_node_type) get_node_type(value) == CARTO_COLOR);
            map.set_background(as<mapnik::color>(value));
        } else if (key == "background-image") {
            map.set_background_image(base+as<std::string>(value));
        } else if (key == "buffer-size") {
            map.set_buffer_size(round(as<double>(value)));
        } else if (key == "base") {
            base = as<std::string>(value); // FIXME - not sure this is correct
        } else if (key == "paths-from-xml") {
            relative_to_xml = as<bool>(value);
        } else if (key == "minimum-version") {
            std::string ver_str = as<std::string>(value);
            params["minimum-version"] = ver_str;
            
            int min_ver = version_from_string(ver_str);
            
            if (min_ver == -1 && strict) {
                throw carto_error(std::string("Invalid version string ") + ver_str, get_location(value));
            } else if (min_ver > MAPNIK_VERSION) {
                throw carto_error(std::string("This map uses features only present in Mapnik version ") + ver_str + " and newer");
            }
        } 
        else if (key == "font-directory") {
            std::string dir = base+as<std::string>(value);
            params["font-directory"] = dir;
            //freetype_engine::register_fonts( ensure_relative_to_xml(dir), false);
        } else {
            key_error(key,node);
        }
    }
    
    map.set_extra_parameters(params);

    return;
}

}

