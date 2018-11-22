-module(gm_options).

-export([
    opt/1
]).

%% Options for interacting with GraphicsMagick function
-spec opt(atom() | tuple()) -> tuple().

opt('+adjoin') ->
    {"+adjoin"};
opt(adjoin) ->
    {"-adjoin"};
opt(auto_orient) ->
    {"-auto-orient"};
opt({background, Color}) ->
    {"-background", ":color", [{color, Color}]};
opt({blur, Radius, Sigma}) ->
    {"-blur", ":radiusx:sigma", [
        {radius, Radius},
        {sigma, Sigma}
    ]};
opt({compose, Operator}) ->
    {"-compose", ":operator", [{operator, Operator}]};
opt(create_directories) ->
    {"-create-directories"};
opt({crop, Width, Height}) ->
    {"-crop", ":widthx:height", [
        {width, Width},
        {height, Height}
    ]};
opt({crop, Width, Height, XOffset, YOffset}) ->
    {"-crop", ":widthx:height+:x_offset+:y_offset", [
        {width, Width},
        {height, Height},
        {x_offset, XOffset},
        {y_offset, YOffset}
    ]};
opt({define, Key}) ->
    {"-define", ":key", [{key, Key}]};
opt({define, Key, Value}) ->
    {"-define", ":key=:value", [
        {key, Key},
        {value, Value}
    ]};
opt({dissolve, Percent}) ->
    {"-dissolve", ":percent", [{percent, Percent}]};
opt({draw, Primitive, XInset, YInset}) ->
    {"-draw", ":primitive :xinset,:yinset", [
        {primitive, Primitive},
        {xinset, XInset},
        {yinset, YInset}
    ]};
opt({draw, Primitive, XInset, YInset, XOffset, YOffset}) ->
    {"-draw", ":primitive :x_inset,:y_inset :x_offset,:y_offset", [
        {primitive, Primitive},
        {x_inset, XInset},
        {y_inset, YInset},
        {x_offset, XOffset},
        {y_offset, YOffset}
    ]};
opt({edge, Radius}) ->
    {"-edge", ":radius", [{radius, Radius}]};
opt({extent, Width, Height}) ->
    {"-extent", ":widthx:height", [
        {width, Width},
        {height, Height}
    ]};
opt(flatten) ->
    {"-flatten"};
opt({fill, Color}) ->
    {"-fill", ":color", [{color, Color}]};
opt(flip) ->
    {"-flip"};
opt({font, Font}) ->
    {"-font", ":font", [{font, Font}]};
opt({format, Format}) ->
    {"-format", ":format", [{format, Format}]};
opt({geometry, Width, Height}) ->
    {"-geometry", ":widthx:height", [
        {width, Width},
        {height, Height}
    ]};
opt({geometry, Width, Height, XOffset, YOffset}) ->
    opt({geometry, Width, Height, XOffset, YOffset, ''});
opt({geometry, Width, Height, XOffset, YOffset, ResizeOption}) ->
    XOffsetOption = case XOffset < 0 of
        true -> erlang:integer_to_list(XOffset);
        false -> "+" ++ erlang:integer_to_list(XOffset)
    end,
    YOffsetOption = case YOffset < 0 of
        true -> erlang:integer_to_list(YOffset);
        false -> "+" ++ erlang:integer_to_list(YOffset)
    end,
    {"-geometry", ":widthx:height:x_offset:y_offset:resize_option", [
        {width, Width},
        {height, Height},
        {x_offset, XOffsetOption},
        {y_offset, YOffsetOption},
        {resize_option, ResizeOption}
    ]};
opt({gravity, Gravity}) ->
    {"-gravity", ":gravity", [{gravity, Gravity}]};
opt({implode, Factor}) ->
    {"-implode", ":factor", [{factor, Factor}]};
opt({interlace, Interlace}) ->
    {"-interlace", ":interlace", [{interlace, Interlace}]};
opt({label, Text}) ->
    {"label:\"" ++ Text ++ "\""};
opt(magnify) ->
    {"-magnify"};
opt('+matte') ->
    {"+matte"};
opt(matte) ->
    {"-matte"};
opt({median, Radius}) ->
    {"-median", ":radius", [{radius, Radius}]};
opt(negate) ->
    {"-negate"};
opt({opaque, Color}) ->
    {"-opaque", ":color", [{color, Color}]};
opt({output_directory, Dir}) ->
    {"-output-directory", ":output_directory", [{output_directory, Dir}]};
opt({pattern, Pattern}) ->
    {"PATTERN:" ++ Pattern};
opt({pointsize, Value}) ->
    {"-pointsize", ":value", [{value, Value}]};
opt({'+profile', Profile}) ->
    {"+profile", ":profile", [
        {profile, Profile}
    ]};
opt({quality, Quality}) ->
    {"-quality", ":quality", [{quality, Quality}]};
opt({'+raise', Width, Height}) ->
    {"+raise", ":widthx:height", [
        {width, Width},
        {height, Height}
    ]};
opt({raise , Width, Height}) ->
    {"-raise", ":widthx:height", [
        {width, Width},
        {height, Height}
    ]};
opt({resize, Width, Height}) ->
    {"-resize", ":widthx:height", [
        {width, Width},
        {height, Height}
    ]};
opt({rotate, Degrees}) ->
    {"-rotate", ":degrees", [{degrees, Degrees}]};
opt({sharpen, Radius}) ->
    {"-sharpen", ":radius", [{radius, Radius}]};
opt({sharpen, Radius, Sigma}) ->
    {"-sharpen", ":radiusx:sigma", [
        {radius, Radius},
        {sigma, Sigma}
    ]};
opt({size, Width, Height}) ->
    {"-size", ":widthx:height", [
        {width, Width},
        {height, Height}
    ]};
opt({spread, Amount}) ->
    {"-spread", ":amount", [{amount, Amount}]};
opt(strip) ->
    {"-strip"};
opt({swirl, Degrees}) ->
    {"-swirl", ":degrees", [{degrees, Degrees}]};
opt({thumbnail, Width, Height}) ->
    {"-thumbnail", ":widthx:height", [
        {width, Width},
        {height, Height}
    ]};
opt({transparent, Color}) ->
    {"-transparent", ":color", [{color, Color}]};
opt({type, Type}) ->
    {"-type", ":type", [{type, Type}]};
opt({watermark, Width, Height}) ->
    {"-watermark", ":widthx:height", [
        {width, Width},
        {height, Height}
    ]};
opt({wave, Amplitude, WaveLength}) ->
    {"-wave", ":amplitudex:wave_length", [
        {amplitude, Amplitude},
        {wave_length, WaveLength}
    ]}.