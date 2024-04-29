#pragma once

enum los_type
{
    LOS_NONE         = 0,        // w g s c m
    LOS_ARENA        = LOS_NONE, // ---------  See key below
    LOS_DEFAULT      = (1 << 0), // o T T h o
    LOS_NO_TRANS     = (1 << 1), // o o T h T*
    LOS_SOLID        = (1 << 2), // o o o T o
    LOS_SOLID_SEE    = (1 << 3), // o o o h T*
    // KEY:
    //   o: opaque, T: transparent, h: half-opaque (two cells block LOS),
    //   T*: transparent but next immediate tile is now opaque (wall mosnters)
    // Columns:
    //   w: FFT_OPAQUE features: rock walls, closed doors, trees, etc.
    //   g: glass (transparent walls)
    //   s: other FFT_SOLID features: grate, statue/idol, open/lava sea
    //   c: semi-opaque clouds (fog, etc.); bushes
    //   m: monsters in walls
};
