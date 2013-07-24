:- module(
  svg_attributes,
  [
    svg_base_profile//3, % -Tree:compound
                         % :DCG_Namespace
                         % ?ProfileName:atom
    svg_content_script_type//3, % -Tree:compound
                                % :DCG_Namespace
                                % ?MediaType:atom
    svg_content_style_type//3, % -Tree:compound
                               % :DCG_Namespace
                               % ?MediaType:atom
    svg_height//4, % -Tree:compound
                   % ?DCG_Namespace
                   % ?Number:float
                   % ?Unit:atom
    svg_preserve_aspect_ratio//5, % -Tree:compound,
                                  % :DCG_Namespace,
                                  % ?Defer:boolean,
                                  % ?Align:compound,
                                  % ?MeetOrSlice:oneof([meet,slice])
    svg_referenced_features//3, % -Tree:compound
                                % ?DCG_Namespace
                                % ?Features:list
    svg_stroke//3, % -Tree:compound
                   % ?DCG_Namespace
                   % ?Color:atom
    svg_stroke_width//4, % -Tree:compound
                         % ?DCG_Namespace
                         % ?Number:float
                         % ?Unit:atom
    svg_version//3, % -Tree:compound
                    % ?DCG_Namespace
                    % ?Version:compound
    svg_width//4, % -Tree:compound
                  % ?DCG_Namespace
                  % ?Number:float
                  % ?Unit:atom
    svg_x//4, % -Tree:compound
              % ?DCG_Namespace
              % ?Number:float
              % ?Unit:atom
    svg_xml_namespace//7, % -Tree:compound
                          % :DCG_Namespace
                          % ?Scheme:atom
                          % ?Authority:compound
                          % ?Path:list(list(atom))
                          % ?Query:atom
                          % ?Fragment:atom
    svg_y//4, % -Tree:compound
              % ?DCG_Namespace
              % ?Number:float
              % ?Unit:atom
    svg_zoom_and_pan//3 % -Tree:compound
                        % :DCG_Namespace
                        % ?Value:oneof([disable,magnify])
  ]
).

/** <module> SVG_ATTRIBUTES

DCGs for SVG datatypes.

@author Wouter Beek
@version 2013/07
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_cardinal)).
:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(rfc(rfc_2396)).
:- use_module(svg(svg)).
:- use_module(svg(svg_datatypes)).
:- use_module(xml(xml_dcg)).

:- meta_predicate(svg_attribute(//,//,//,?,?)).
:- meta_predicate(svg_attribute(//,//,//,//,?,?)).
:- meta_predicate(svg_base_profile(-,//,?,?,?)).
:- meta_predicate(svg_content_script_type(-,//,?,?,?)).
:- meta_predicate(svg_content_style_type(-,//,?,?,?)).
:- meta_predicate(svg_height(-,//,?,?,?,?)).
:- meta_predicate(svg_preserve_aspect_ratio(-,//,?,?,?,?,?)).
:- meta_predicate(svg_referenced_features(-,//,?,?,?)).
:- meta_predicate(svg_stroke(-,//,?,?,?)).
:- meta_predicate(svg_stroke_width(-,//,?,?,?,?)).
:- meta_predicate(svg_version(-,//,?,?,?)).
:- meta_predicate(svg_width(-,//,?,?,?,?)).
:- meta_predicate(svg_x(-,//,?,?,?,?)).
:- meta_predicate(svg_xml_namespace(-,//,?,?,?,?,?,?,?)).
:- meta_predicate(svg_y(-,//,?,?,?,?)).
:- meta_predicate(svg_zoom_and_pan(-,//,?,?,?)).



svg_attribute(DCG_Namespace, DCG_Name, DCG_Value) -->
  xml_attribute(svg_namespace(DCG_Namespace), DCG_Name, DCG_Value).

svg_attribute(DCG_Namespace, DCG_Name, DCG_Value, DCG_Separator) -->
  xml_attribute(
    svg_namespace(DCG_Namespace),
    DCG_Name,
    DCG_Value,
    DCG_Separator
  ).

%! svg_base_profile(-Tree:compound, :DCG_Namespace, ?ProfileName:atom)//
% ?ProfileName:atom)//Describes the minimum SVG language profile
% that the author believes is necessary to correctly render the content.
% The attribute does not specify any processing restrictions; It can be
% considered metadata. For example, the value of the attribute could be
% used by an authoring tool to warn the user when they are modifying the
% document beyond the scope of the specified base profile. Each SVG
% profile should define the text that is appropriate for this attribute.
%
% If the attribute is not specified, the effect is as if a value of `none`
% were specified.

svg_base_profile(base_profile(T1), DCG_Namespace, ProfileName) -->
  svg_attribute(
    DCG_Namespace,
    dcg_word(baseProfile),
    svg_profile_name(T1, ProfileName)
  ).

%! svg_content_script_type(-Tree:compound, :DCG_Namespace, ?MediaType:atom)//
% Identifies the default scripting language for the given SVG document
% fragment. This attribute sets the default scripting language used
% to process the value strings in event attributes. This language must be
% used for all instances of script that do not specify their own scripting
% language.
%
% ~~~
% contentScriptType = "content-type"
% ~~~
%
% The value specifies a media type, per MIME Part Two: Media Types [RFC2046].
% The default value is =application/ecmascript= [RFC4329].

svg_content_script_type(
  content_script_type(MediaType),
  DCG_Namespace,
  MediaType
) -->
  svg_attribute(
    DCG_Namespace,
    dcg_word(contentScriptType),
    svg_content_type(MediaType)
  ).

%! svg_content_style_type(-Tree:compound, :DCG_Namespace, ?MediaType:atom)//
% Identifies the default style sheet language for the given document.
% That language must then be used for all instances of style that do not
% specify their own style sheet language.
%
% ~~~
% contentStyleType = "content-type"
% ~~~
%
% The value specifies a media type, per MIME Part Two: Media Types [RFC2046].
% The default value is `text/css` [RFC2318].

svg_content_style_type(
  content_style_type(MediaType),
  DCG_Namespace,
  MediaType
) -->
  svg_attribute(
    DCG_Namespace,
    dcg_word(contentStyleType),
    svg_content_type(MediaType)
  ).

%! svg_height(-Tree:compound, :DCG_Namespace, ?Number:float, ?Unit:atom)//
% For **outermost SVG elements**, the intrinsic width of
% the SVG document fragment.
% For **embedded SVG elements**, the height of the rectangular region
% into which the SVG element is placed.
%
% A negative value is an error.
% A value of zero disables rendering of the element.
% If the attribute is not specified, the effect is as if a value of `100%`
% were specified.

svg_height(height(T1), DCG_Namespace, Number, Unit) -->
  svg_attribute(DCG_Namespace, dcg_word(height), svg_length(T1, Number, Unit)).

%! svg_preserve_aspect_ratio(
%!   -Tree:compound,
%!   :DCG_Namespace,
%!   ?Defer:boolean,
%!   ?Align:compound,
%!   ?MeetOrSlice:oneof([meet,slice])
%! )//
% Can be used to stretch graphics so as to fit non-uniformly to take up
% the entire viewport, or to set that uniform scaling be used for
% the purposes of preserving the aspect ratio of the graphics.
%
% ~~~
% preserveAspectRatio="[defer] <align> [<meetOrSlice>]"
% ~~~
%
% If the attribute is not specified, then the effect is as if a value
% of `xMidYMid meet` were specified.
%
% ## Viewport
%
% On all elements that establish a new viewport this attribute indicates
% whether or not to force uniform scaling.
%
% For elements that establish a new viewport, plus `marker`, `pattern` and
% `view` elements, svg_preserve_aspect_ratio// only applies when a value
% has been provided for svg_view_box// on the same element.
% Othewise svg_preserve_aspect_ratio// is ignored.
%
% ## Image
%
% For `image` elements this indicates how referenced images should be fitted
% with respect to the reference rectangle and whether the aspect ratio
% of the referenced image should be preserved with respect to
% the current user coordinate system.
%
% If the svg_preserve_aspect_ratio// on an svg_image// starts with `defer`
% then the value of the preserve aspect ratio attribute on
% the referenced content -- if present -- should be used.
% If the referenced content lacks this value then the attribute should be
% processed as normal (ignoring `defer`).
%
% On all other elements the `defer` portion of the attribute is ignored.
%
% ## Example
%
% [[svg_preserve_aspect_ratio.png]]
%
% @arg Tree A compound term representing a parse tree.
% @arg Defer A boolean representing whether the preserve aspect radio
%      attribute of the referenced content should be used instead
%      (in case this is present).
% @arg Align A compound term of the following form:
%      =|align(x(?X:oneof([min,mid,max,none])),y(?Y:oneof([min,mid,max,none])))|=
% @arg MeetOrSlide Either uninstantiated, `meet`, or `slice`.
%
% @see This attribute uses parameters svg_defer//2, svg_align//3,
%      and svg_meet_or_slice//2.

svg_preserve_aspect_ratio(
  preserve_aspect_ratio(T1,T2,T3),
  DCG_Namespace,
  Defer,
  align(XAlign,YAlign),
  MeetOrSlice
) -->
  svg_attribute(
    DCG_Namespace,
    dcg_word(preserveAspectRatio),
    (
      svg_defer(T1, Defer),
      svg_align(T2, XAlign, YAlign),
      svg_meet_or_slice(T3, MeetOrSlice)
    )
  ).

%! svg_referenced_features(-Tree:compound, :DCG_Namespace, ?Features:list)//
% ~~~
% requiredFeatures = list-of-features
% ~~~
%
% The features are a list of strings separated by white space.
% All features must be supported by the user agent in order for this
% attribute to evaluate to `true`.
% If it evaluates to `false`, then the current element and its children are
% skipped / not rendered.
%
% Evaluates to `true` if absent.
%
% Evaluates to `false` if one of the features is the null or empty string.
%
% Without a `switch` element, this attribute represents a simple switch
% on the given element whether to render the element or not.
%
% @tbd Add feature strings.

svg_referenced_features(T0, DCG_Namespace, Features) -->
  svg_attribute(
    DCG_Namespace,
    dcg_word(referencedFeatures),
    svg_feature_string(Trees, Features),
    space
  ),
  {parse_tree(referenced_features, Trees, T0)}.

% @tbd STUB!
svg_feature_string(feature_string(Feature), Feature) --> dcg_word(Feature).

%svg_required_extensions(required_extensions(T1),

svg_stroke(stroke(T1), DCG_Namespace, Color) -->
  svg_attribute(DCG_Namespace, dcg_word(stroke), svg_color(T1, Color)).

svg_stroke_width(stroke_width(T1), DCG_Namespace, Number, Unit) -->
  svg_attribute(
    DCG_Namespace,
    (dcg_word(stroke), hyphen_minus, dcg_word(width)),
    svg_length(T1, Number, Unit)
  ).

%! svg_version(-Tree:compound, :DCG_Namespace, ?Version:compound)//
% Indicates the SVG language version to which this document fragment conforms.
% In SVG 1.0, this attribute was fixed to the value `1.0`.
% For SVG 1.1, the attribute should have the value `1.1`.
%
% @arg Version A compound term of the form
%      =|version(major(?Major:integer),minor(?Minor:integer))|=.

svg_version(
  version(major(Major),'.',minor(Minor)),
  DCG_Namespace,
  version(Major,Minor)
) -->
  svg_attribute(
    DCG_Namespace,
    dcg_word(version),
    (
      decimal_number(Major),
      dot,
      decimal_number(Minor)
    )
  ).

%! svg_width(-Tree:compound, :DCG_Namespace, ?Number:float, ?Unit:atom)//
% For **outermost SVG elements**, the intrinsic width of
% the SVG document fragment.
% For **embedded SVG elements**, the width of the rectangular region into which
% the SVG element is placed.
%
% A negative value is an error.
% A value of zero disables rendering of the element.
% If the attribute is not specified, the effect is as if a value of `100%`
% were specified.

svg_width(width(T1), DCG_Namespace, Number, Unit) -->
  svg_attribute(DCG_Namespace, dcg_word(width), svg_length(T1, Number, Unit)).

%! svg_x(-Tree:compound, :DCG_Namespace, ?Number:float, ?Unit:atom)//
% The x-axis coordinate of one corner of the rectangular region
% into which an embedded ‘svg’ element is placed.
%
% If the attribute is not specified, the effect is as if a value of `0`
% were specified.

svg_x(x(T1), DCG_Namespace, Number, Unit) -->
  svg_attribute(DCG_Namespace, dcg_word(x), svg_coordinate(T1, Number, Unit)).

%! svg_xml_namespace(
%!   -Tree:compound,
%!   :DCG_Namespace,
%!   ?Scheme:atom,
%!   ?Authority:compound,
%!   ?Path:list(list(atom)),
%!   ?Query:atom,
%!   ?Fragment:atom
%! )//

svg_xml_namespace(
  xml_namespace(T1),
  DCG_Namespace,
  Scheme,
  Authority,
  Path,
  Query,
  Fragment
) -->
  svg_attribute(
    DCG_Namespace,
    dcg_word(xmlns),
    uri_reference(
      T1,
      Scheme,
      Authority,
      Path,
      Query,
      Fragment
    )
  ).

%! svg_y(-Tree:compound, :DCG_Namespace, ?Number:float, ?Unit:atom)//
% The y-axis coordinate of one corner of the rectangular region
% into which an embedded `svg` element is placed.
%
% If the attribute is not specified, the effect is as if a value of `0`
% were specified.

svg_y(y(T1), DCG_Namespace, Number, Unit) -->
  svg_attribute(DCG_Namespace, dcg_word(y), svg_coordinate(T1, Number, Unit)).

%! svg_zoom_and_pan(
%!   -Tree:compound,
%!   :DCG_Namespace,
%!   ?Value:oneof([disable,magnify])
%! )//
% The outermost svg element in an SVG document fragment has this attribute.
% The following values are supported:
%   1. `disable`
%      The user agent shall disable any magnification and panning controls
%      and not allow the user to magnify or pan on the given
%      document fragment.
%   2. `magnify` (the default)
%      In environments that support user interactivity, the user agent
%      shall provide controls to allow the user to perform a magnify
%      operation on the document fragment.

svg_zoom_and_pan(zoom_and_pan(Value), DCG_Namespace, Value) -->
  {member(Value, [disable,magnify])},
  svg_attribute(DCG_Namespace, dcg_word(zoomAndPan), dcg_word(Value)).

