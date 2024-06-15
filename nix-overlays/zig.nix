final: prev:
{
  zig-asahi = prev.zig_0_12.overrideAttrs (old: {
    patches = (old.patches or []) ++ [
      ./zig-asahi.patch
    ];
  });
}
