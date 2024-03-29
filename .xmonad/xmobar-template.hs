-- ʕっ•ᴥ•ʔっ HEADER START
--
-- THIS IS A TEMPLATE FILE.
-- Run ~/.scripts/colour-manager.py to convert this into a valid xmobar.conf
--
-- ʕっ•ᴥ•ʔっ HEADER END

-- This is mostly from Ethan Schoonover:
-- https://github.com/altercation/dotfiles-tilingwm/blob/master/.xmonad/xmobar.conf

-- weather:
-- https://www.faa.gov/air_traffic/weather/asos
-- http://forecast.weather.gov/MapClick.php?textField1=47.66&textField2=-122.35#.WIjEN0fytec
--, additionalFonts    = ["xft:Raleway:size=12:antialias=true:hinting=true", "xft:Inconsolata for Powerline:size=12:antialias=true:hinting=true"]

Config

    {
      -- font              = "xft:Input Sans Compressed:size=10:antialias=true,Font Awesome 5 Free:style=Solid:size=9:hinting=true"
      font              = "xft:RobotoMono Nerd Font:size=10:antialias=true,Font Awesome 5 Free:style=Solid:size=9:hinting=true"
    , allDesktops       = True

    , bgColor           = "+background+"
    , fgColor           = "+foreground+"
    -- workspace indicators don't work right if alpha is turned on?!
    -- , alpha             = 200
    , overrideRedirect  = True
    , commands           = [
          Run StdinReader
        , Run Com "/home/hives/.scripts/whats-playing" [] "nowplaying" 10
        -- , Run Volume "default" "Master"
        --    [ "-t", "<status>", "--"
        --    , "--on", "<fc=+green+> <volume>%</fc>"
        --    , "--onc", "+green+"
        --    , "--off", "<fc=+red_bright+> MUTE</fc>"
        --    , "--offc", "+red_bright+"
        --    ] 1
        , Run Cpu
            [ "-t"," C/M <vbar>"
            --, "-p", "2"
            , "-L", "40"
            , "-H", "60"
            , "-l", "+white+"
            , "-h", "+red_bright+"
            ] 10
        , Run Memory
            [ "-t", "<usedvbar>"
            , "-p", "2"
            , "-l", "+white+"
            , "-h", "+blue+"
            ] 10
        , Run Com "/home/hives/.scripts/status-net" [] "net" 50
        , Run Battery
            [ "-t", "<fc=+yellow+><acstatus></fc>"
            , "-L", "20"
            , "-H", "85"
            , "-l", "+red_bright+"
            , "-n", "+yellow+"
            , "-h", "+green+"
            , "--" -- battery specific options
            -- discharging status
            , "-o"  , " <left>% (<timeleft>) <watts>w"
            -- AC "on" status
            , "-O"  , " <left>%"
            -- charged status
            , "-i"  , " <left>%"
            , "--off-icon-pattern", ""
            , "--on-icon-pattern", ""
            ] 10
        -- , Run Weather "EGLC" -- London City Airport
        --     [ "--template", "<fc=+green+> <tempC>°C</fc>" -- green
        --     ] 600
        , Run Date "<fc=+blue+> %A %d %B</fc> | <fc=+blue+> %l:%M %P</fc>" "date" 10
        -- , Run Network "enp0s31f6"
        --     [ "-t", " <fc=#6c71c4>\xf065 ETH<rxipat></fc>"
        --     ] 10 
        -- , Run DynNetwork
        --     [ "-t", "<fc=#6c71c4>| <dev></fc>"
        --     ] 10 
        -- , Run Com "status-vol" [] "volume" 5
        -- , Run Com "status-keyboard" [] "kb" 10
        ]
        , sepChar            = "%"
        , alignSep           = "}{"
        , template           = "%StdinReader% }{ %nowplaying%%cpu%%memory% | %net% | %battery% | | %date% "
        -- , template           = "%StdinReader% }{ %nowplaying%%cpu%%memory% | %net% | %battery% | %EGLC% | %date% "
        -- , template           = "%StdinReader% }{ %nowplaying%%default:Master% | %cpu%%memory% | %net% | %battery% | %EGLC% | %date% "
    }

-- not really haskell, but close enough
-- vim: ft=haskell:foldmethod=marker:expandtab:ts=4:shiftwidth=4
