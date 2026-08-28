# Shape Lab

An interactive, full-color 3D shape viewer for the terminal, written in modern Fortran. It renders five lit meshes with smooth motion, depth buffering, shadows, themed environments, and mouse controls directly in a true-color TUI.

## Build and run

You need `gfortran` and a terminal with ANSI true-color and SGR mouse support.

```bash
make
./shapes
```

## Controls

| Input | Action |
| --- | --- |
| Mouse drag | Free-form virtual-trackball rotation |
| Mouse wheel | Zoom in or out |
| Arrow keys / `WASD` | Rotate |
| `1`–`5` | Select a shape |
| `N` | Select the next shape |
| `T` | Cycle color themes |
| `Space` | Pause or resume auto-rotation |
| `[` / `]` | Decrease or increase spin speed |
| `Z` / `X` | Zoom in or out |
| `R` | Reset the view |
| `H` | Hide or show the interface |
| `Q` | Quit |

Shape Lab restores the normal screen, cursor, and mouse behavior when it exits.
