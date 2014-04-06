:- module(
  svg_colors,
  [
    svg_color/2, % ?ColorName:atom
                 % ?RGB:rgb
    svg_colors/1 % -Colors:list(atom)
  ]
).

/** <module> SVG_COLORS

The SVG color names.

@author Wouter Beek
@version 2013/07
*/



%! svg_color(?ColorName:atom, ?RGB:rgb) is nondet.

svg_color(aliceblue, rgb(240, 248, 255)).
svg_color(antiquewhite, rgb(250, 235, 215)).
svg_color(aqua, rgb(0, 255, 255)).
svg_color(aquamarine, rgb(127, 255, 212)).
svg_color(azure, rgb(240, 255, 255)).
svg_color(beige, rgb(245, 245, 220)).
svg_color(bisque, rgb(255, 228, 196)).
svg_color(black, rgb(0, 0, 0)).
svg_color(blanchedalmond, rgb(255, 235, 205)).
svg_color(blue, rgb(0, 0, 255)).
svg_color(blueviolet, rgb(138, 43, 226)).
svg_color(brown, rgb(165, 42, 42)).
svg_color(burlywood, rgb(222, 184, 135)).
svg_color(cadetblue, rgb(95, 158, 160)).
svg_color(chartreuse, rgb(127, 255, 0)).
svg_color(chocolate, rgb(210, 105, 30)).
svg_color(coral, rgb(255, 127, 80)).
svg_color(cornflowerblue, rgb(100, 149, 237)).
svg_color(cornsilk, rgb(255, 248, 220)).
svg_color(crimson, rgb(220, 20, 60)).
svg_color(cyan, rgb(0, 255, 255)).
svg_color(darkblue, rgb(0, 0, 139)).
svg_color(darkcyan, rgb(0, 139, 139)).
svg_color(darkgoldenrod, rgb(184, 134, 11)).
svg_color(darkgray, rgb(169, 169, 169)).
svg_color(darkgreen, rgb(0, 100, 0)).
svg_color(darkgrey, rgb(169, 169, 169)).
svg_color(darkkhaki, rgb(189, 183, 107)).
svg_color(darkmagenta, rgb(139, 0, 139)).
svg_color(darkolivegreen, rgb(85, 107, 47)).
svg_color(darkorange, rgb(255, 140, 0)).
svg_color(darkorchid, rgb(153, 50, 204)).
svg_color(darkred, rgb(139, 0, 0)).
svg_color(darksalmon, rgb(233, 150, 122)).
svg_color(darkseagreen, rgb(143, 188, 143)).
svg_color(darkslateblue, rgb(72, 61, 139)).
svg_color(darkslategray, rgb(47, 79, 79)).
svg_color(darkslategrey, rgb(47, 79, 79)).
svg_color(darkturquoise, rgb(0, 206, 209)).
svg_color(darkviolet, rgb(148, 0, 211)).
svg_color(deeppink, rgb(255, 20, 147)).
svg_color(deepskyblue, rgb(0, 191, 255)).
svg_color(dimgray, rgb(105, 105, 105)).
svg_color(dimgrey, rgb(105, 105, 105)).
svg_color(dodgerblue, rgb(30, 144, 255)).
svg_color(firebrick, rgb(178, 34, 34)).
svg_color(floralwhite, rgb(255, 250, 240)).
svg_color(forestgreen, rgb(34, 139, 34)).
svg_color(fuchsia, rgb(255, 0, 255)).
svg_color(gainsboro, rgb(220, 220, 220)).
svg_color(ghostwhite, rgb(248, 248, 255)).
svg_color(gold, rgb(255, 215, 0)).
svg_color(goldenrod, rgb(218, 165, 32)).
svg_color(gray, rgb(128, 128, 128)).
svg_color(grey, rgb(128, 128, 128)).
svg_color(green, rgb(0, 128, 0)).
svg_color(greenyellow, rgb(173, 255, 47)).
svg_color(honeydew, rgb(240, 255, 240)).
svg_color(hotpink, rgb(255, 105, 180)).
svg_color(indianred, rgb(205, 92, 92)).
svg_color(indigo, rgb(75, 0, 130)).
svg_color(ivory, rgb(255, 255, 240)).
svg_color(khaki, rgb(240, 230, 140)).
svg_color(lavender, rgb(230, 230, 250)).
svg_color(lavenderblush, rgb(255, 240, 245)).
svg_color(lawngreen, rgb(124, 252, 0)).
svg_color(lemonchiffon, rgb(255, 250, 205)).
svg_color(lightblue, rgb(173, 216, 230)).
svg_color(lightcoral, rgb(240, 128, 128)).
svg_color(lightcyan, rgb(224, 255, 255)).
svg_color(lightgoldenrodyellow, rgb(250, 250, 210)).
svg_color(lightgray, rgb(211, 211, 211)).
svg_color(lightgreen, rgb(144, 238, 144)).
svg_color(lightgrey, rgb(211, 211, 211)).
svg_color(lightpink, rgb(255, 182, 193)).
svg_color(lightsalmon, rgb(255, 160, 122)).
svg_color(lightseagreen, rgb(32, 178, 170)).
svg_color(lightskyblue, rgb(135, 206, 250)).
svg_color(lightslategray, rgb(119, 136, 153)).
svg_color(lightslategrey, rgb(119, 136, 153)).
svg_color(lightsteelblue, rgb(176, 196, 222)).
svg_color(lightyellow, rgb(255, 255, 224)).
svg_color(lime, rgb(0, 255, 0)).
svg_color(limegreen, rgb(50, 205, 50)).
svg_color(linen, rgb(250, 240, 230)).
svg_color(magenta, rgb(255, 0, 255)).
svg_color(maroon, rgb(128, 0, 0)).
svg_color(mediumaquamarine, rgb(102, 205, 170)).
svg_color(mediumblue, rgb(0, 0, 205)).
svg_color(mediumorchid, rgb(186, 85, 211)).
svg_color(mediumpurple, rgb(147, 112, 219)).
svg_color(mediumseagreen, rgb(60, 179, 113)).
svg_color(mediumslateblue, rgb(123, 104, 238)).
svg_color(mediumspringgreen, rgb(0, 250, 154)).
svg_color(mediumturquoise, rgb(72, 209, 204)).
svg_color(mediumvioletred, rgb(199, 21, 133)).
svg_color(midnightblue, rgb(25, 25, 112)).
svg_color(mintcream, rgb(245, 255, 250)).
svg_color(mistyrose, rgb(255, 228, 225)).
svg_color(moccasin, rgb(255, 228, 181)).
svg_color(navajowhite, rgb(255, 222, 173)).
svg_color(navy, rgb(0, 0, 128)).
svg_color(oldlace, rgb(253, 245, 230)).
svg_color(olive, rgb(128, 128, 0)).
svg_color(olivedrab, rgb(107, 142, 35)).
svg_color(orange, rgb(255, 165, 0)).
svg_color(orangered, rgb(255, 69, 0)).
svg_color(orchid, rgb(218, 112, 214)).
svg_color(palegoldenrod, rgb(238, 232, 170)).
svg_color(palegreen, rgb(152, 251, 152)).
svg_color(paleturquoise, rgb(175, 238, 238)).
svg_color(palevioletred, rgb(219, 112, 147)).
svg_color(papayawhip, rgb(255, 239, 213)).
svg_color(peachpuff, rgb(255, 218, 185)).
svg_color(peru, rgb(205, 133, 63)).
svg_color(pink, rgb(255, 192, 203)).
svg_color(plum, rgb(221, 160, 221)).
svg_color(powderblue, rgb(176, 224, 230)).
svg_color(purple, rgb(128, 0, 128)).
svg_color(red, rgb(255, 0, 0)).
svg_color(rosybrown, rgb(188, 143, 143)).
svg_color(royalblue, rgb(65, 105, 225)).
svg_color(saddlebrown, rgb(139, 69, 19)).
svg_color(salmon, rgb(250, 128, 114)).
svg_color(sandybrown, rgb(244, 164, 96)).
svg_color(seagreen, rgb(46, 139, 87)).
svg_color(seashell, rgb(255, 245, 238)).
svg_color(sienna, rgb(160, 82, 45)).
svg_color(silver, rgb(192, 192, 192)).
svg_color(skyblue, rgb(135, 206, 235)).
svg_color(slateblue, rgb(106, 90, 205)).
svg_color(slategray, rgb(112, 128, 144)).
svg_color(slategrey, rgb(112, 128, 144)).
svg_color(snow, rgb(255, 250, 250)).
svg_color(springgreen, rgb(0, 255, 127)).
svg_color(steelblue, rgb(70, 130, 180)).
svg_color(tan, rgb(210, 180, 140)).
svg_color(teal, rgb(0, 128, 128)).
svg_color(thistle, rgb(216, 191, 216)).
svg_color(tomato, rgb(255, 99, 71)).
svg_color(turquoise, rgb(64, 224, 208)).
svg_color(violet, rgb(238, 130, 238)).
svg_color(wheat, rgb(245, 222, 179)).
svg_color(white, rgb(255, 255, 255)).
svg_color(whitesmoke, rgb(245, 245, 245)).
svg_color(yellow, rgb(255, 255, 0)).
svg_color(yellowgreen, rgb(154, 205, 50)).

%! svg_colors(-Colors:list(atom)) is det.
% Returns the list with supported color names.
%
% @arg Colors A list with atomic color names.

svg_colors(Colors):-
  findall(Color, svg_color(Color, _RGB), Colors).
