# eww status bar for weir

A complete [eww](https://github.com/elkowar/eww) bar with weir workspace
integration: clickable per-workspace buttons, a volume slider, battery,
clock (click to toggle a calendar popup), wifi, bluetooth, and a system
tray.

```
[1][2][3]...                    [vol ====] wifi bt 87% [tray] Mon 06-01 14:32
```

The center is intentionally left empty to avoid laptop display notches.

Tested against eww 0.6.0 + river 0.4.5 + weir (headless CI harness:
rendering, exclusive zone, and live workspace updates verified).

## Dependencies

- eww >= 0.6 (the systray widget requires 0.6)
- `weirctl` and `jq` (workspaces)
- `wpctl` / wireplumber (volume)
- `pactl` (pkgs.pulseaudio CLI) — for event-driven volume updates; without
  it the volume falls back to 10s polling
- `nmcli` / NetworkManager (wifi) — optional, widget shows "no wifi" without it
- `bluetoothctl` / bluez (bluetooth) — optional
- A session dbus (for the systray and bluetooth events)

Every script degrades gracefully when its backing service is missing.

## Power usage

All data sources are event-driven (`deflisten` on pactl/nmcli/bluez/weirctl
streams) rather than polled: at idle the bar causes ~3.5 process wakeups per
minute (the once-a-minute clock and battery reads, plus slow refresh ticks)
instead of the ~50/min a naive polling setup costs. Volume, wifi, bluetooth,
and workspace changes appear instantly rather than on the next poll.


## Customizing

- **Colors / sizing:** `eww.scss`. The defaults match a dark bar with
  purple accents (#5f27cd focused workspace).
- **Click targets:** the wifi widget opens `nm-connection-editor`, bluetooth
  opens `blueman-manager` — edit the `:onclick` in `eww.yuck` to taste.
- **Adding widgets:** add a `defpoll`/`deflisten` + a `defwidget` and drop it
  into `bar-layout`. See the existing widgets for the pattern; the eww
  documentation covers the rest.
- **Multi-monitor:** duplicate the `defwindow` block with `:monitor 1` and a
  different name (`bar2`), then `eww open bar2` as well.

## How the workspace integration works

`scripts/weir-workspaces-json` turns `weirctl subscribe` into a stream of
JSON arrays (one per state change):

```json
[{"name":"1","class":"focused"},{"name":"2","class":"occupied"},...]
```

eww's `deflisten` feeds that to a `for` loop that renders one button per
workspace with the class as its CSS class — focused / visible / occupied /
empty. Clicking a button runs `weirctl view <name>`.
