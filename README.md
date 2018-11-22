# erl_gm

An Erlang GraphicsMagick wrapper

## Erlang usage

```erlang
% Crop image into 100x100 tiles
gm:convert("/some/image.jpg", "tile", [{crop, 100, 100}]).

% Get multiple identify properties (returns a map of characteristics)
gm:identify_explicit("/some/image.jpg", [width, height, filename, type]).

% Crazytown
gm:convert("/some/image.jpg", "/something/crazy.jpg", [
  flip,
  magnify,
  {rotate, 45},
  {blur, 7, 3},
  {crop, 300, 300, 150, 130},
  {edge, 3}
]).

% Resize
gm:convert("/some/image.jpg", "/something/resized.jpg", [{resize, 240, 240}]).
```

## Elixir installation

Add erl_gm to your `mix.exs` dependencies:

```elixir
def deps do
  [{:gm, git: "https://github.com/pmyarchon/erl_gm"}]
end
```

List the `:gm` application as your application dependency (from Elixir version 1.4.0 may not be necessary)

```elixir
def application do
  [applications: [:gm]]
end
```

## Elixir usage

```elixir
iex> :gm.convert('/some/image.jpg', '/some/cropped.jpg', [{:crop, 100, 100}])
```

## Available GM functions

```
gm:composite
gm:convert
gm:identify
gm:mogrify
gm:montage
gm:version
```

## Availavle GM options

```
+adjoin
-adjoin
-auto-orient
-background
-blur
-compose
-create_directories
-crop
-define
-dissolve
-draw
-edge
-extent
-flatten
-fill
-flip
-font
-format
-geometry
-gravity
-implode
-interlace
-magnify
+matte
-matte
-median
-negate
-opaque
-output_directory
-pointsize
+profile
-quality
+raise
-raise
-resize
-rotate
-sharpen
-size
-spread
-strip
-swirl
-thumbnail
-transparent
-type
-watermark
-wave
label
PATTERN
```

## Available shortcut functions

```
gm:identify_explicit
```

## License

MIT
