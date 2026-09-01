-- See https://wiki.hypr.land/Configuring/Basics/Monitors/
hl.monitor({
    output   = "DP-2",
    mode     = "preferred",
    position = "0x0",
    scale    = "1",
})

hl.monitor({
    output   = "DP-3",
    mode     = "preferred",
    position = "auto-right",
    scale    = "1",
    transform = 3,
})

hl.monitor({
    output   = "",
    mode     = "preferred",
    position = "auto",
    scale    = "1",
})
