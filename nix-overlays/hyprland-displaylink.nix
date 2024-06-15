final: prev:
{
  hyprland-displaylink = prev.hyprland.overrideAttrs(oldAttrs: rec {
    prePatch = ''
      rm subprojects/wlroots-hyprland/patches/nvidia-hardware-cursors.patch
    '';
    patches = (oldAttrs.patches or []) ++ [./displaylink.patch];
  });
}
