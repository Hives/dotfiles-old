import XMonad
import qualified XMonad.Actions.ConstrainedResize as Sqr
import XMonad.Actions.CopyWindow
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.FloatKeys
import XMonad.Actions.FloatSnap
import XMonad.Actions.MessageFeedback       -- pseudo conditional key bindings
import XMonad.Actions.Navigation2D
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.Promote
import XMonad.Actions.CycleWS
import XMonad.Actions.WithAll
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Accordion
import XMonad.Layout.BinarySpacePartition
-- import qualified XMonad.Layout.BoringWindows
import XMonad.Layout.Dwindle
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.Hidden
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowNavigation
import XMonad.ManageHook
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Input
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.Paste as P               -- testing
import XMonad.Util.Run
import XMonad.Util.Themes
import XMonad.Util.WorkspaceCompare
import qualified XMonad.StackSet as W
import Data.Char
import Data.List
import Data.Time
import Data.Ratio ((%)) -- required for XMonad.Layout.IM
import Graphics.X11.Xinerama

import System.Exit
import System.IO

import qualified Data.Map as M
-- import qualified XMonad.Actions.Submap as SM
import XMonad.Actions.Search as S

------------------------------------------------------------------------
-- Colours
------------------------------------------------------------------------

-- colours from colour manager start here
-- don't fiddle with these lines or it might mess everything up

-- ʕっ•ᴥ•ʔっ COLOURS START

-- /home/hives/.config/xcolors/dark-scheme

cText = "#889091"
cActive = "#d33682"
cBackground = "#0e2329"
cVisible = "#162d33"
cDeselected = "#162d33"
cVisibleWorkspaceText = "#979e9e"
cVisibleWorkspaceBackground = "#162d33"
cUrgent = "#dc322f"
cActiveTabText = "#162d33"
cPrompt = "#162d33"
cPromptHighlight = "#268bd2"
cHotPrompt = "#dc322f"
cHotPromptText = "#0e2329"

-- ʕっ•ᴥ•ʔっ COLOURS END

------------------------------------------------------------------------
-- Theme
------------------------------------------------------------------------

border    = 5   -- width of borders
numIcons  = 7   -- width of system tray in icons

topbar    = 8  -- height of top bar
gutter    = 8   -- spacing around windows

topBarTheme = def
    { fontName                = myFont
    , inactiveBorderColor     = cVisible
    , inactiveColor           = cVisible
    , inactiveTextColor       = cVisible
    , activeBorderColor       = cActive
    , activeColor             = cActive
    , activeTextColor         = cActive
    , urgentBorderColor       = cUrgent
    , urgentTextColor         = cUrgent
    , decoHeight              = topbar
    }

myTabTheme = (theme donaldTheme)
    { activeColor             = cActive
    , activeBorderColor       = cActive
    , activeTextColor         = cActiveTabText

    , inactiveColor           = cVisible
    , inactiveBorderColor     = cVisible
    , inactiveTextColor       = cText 

    , fontName                = myFont
    }

myPromptTheme                 = def
    { bgColor                 = cPrompt
    , fgColor                 = cText
    , bgHLight                = cPromptHighlight
    , fgHLight                = cText
    , borderColor             = cPromptHighlight
    , promptBorderWidth       = 0--border
    , position                = Top
    , font                    = myPromptFont 
    , height                  = 25
    , promptKeymap            = defaultXPKeymap' isWordSeparator
    } where isWordSeparator c = isSpace c || c == '/'

hotPromptTheme                = myPromptTheme
    { bgColor                 = cHotPrompt
    , fgColor                 = cHotPromptText
    }

myShowWNameTheme = def
    { swn_font                = myBIGFont
    , swn_fade                = 0.5
    , swn_bgcolor             = "#000000"
    , swn_color               = "#FFFFFF"
    }

myFont       = "xft:Input Sans Compressed:size=10:antialias=true:hinting=true"
myPromptFont = "xft:Input Sans Compressed:size=10:antialias=true:hinting=true"
myBIGFont    = "xft:Input Sans:style=Bold:pixelsize=180:antialias=true:hinting=true"
-- myBIGFont    = "xft:Eurostar Black Extended:style=Regular:pixelsize=180:hinting=true"

myFocusFollowsMouse  = True
myClickJustFocuses   = True

------------------------------------------------------------------------
-- Applications
------------------------------------------------------------------------

myTerminal    = "urxvtc"
-- myLauncher    = "rofi -modi \"drun,window,ssh\" -show drun -scroll-method 1 -show-icons true"
myLauncher    = "rofi -modi \"drun,window,ssh\" -show drun -scroll-method 1"
myFileBrowser = "urxvtc -e ranger"

jellyBrowser  = "dex $HOME/.local/share/applications/chromium-jelly.desktop"
jellySlack    = "dex $HOME/.local/share/applications/chromium-slack-jelly.desktop"
jellySkype    = "dex $HOME/.local/share/applications/chromium-skype-jelly.desktop"
-- jellySkype    = "skypeforlinux"
jellyEmail    = "thunderbird"
jellyNotes    = "urxvtc -e emacs -nw $HOME/Documents/Jellymould/to-do.org"

scratchpads =
    [ NS "terminal"     (myTerminal ++ " -name terminal-scratchpad") (resource =? "terminal-scratchpad") (placeWindow 0.05 0.02 0.52 0.02)
    -- , NS "downloads"    (myTerminal ++ " -name terminal-scratchpad -cd $HOME/Downloads") (resource =? "terminal-scratchpad") (placeWindow 0.05 0.02 0.52 0.02)
    , NS "htop"         "urxvtc -name htop-scratchpad -e htop" (resource =? "htop-scratchpad") (centerScreen 0.7 0.7)
    , NS "alsamixer"    "urxvtc -e alsamixer" (title =? "alsamixer") (centerScreen 0.6 0.7)
    -- , NS "musicplayer"  "urxvtc -name ncmpcpp -e poop" (resource =? "ncmpcpp") (centerScreen 0.7 0.7)
    , NS "musicplayer"  "/usr/bin/mocp-scrobbler.py -d; urxvtc -name mocp -e mocp" (resource =? "mocp") (centerScreen 0.7 0.7)
    , NS "calculator"   "speedcrunch" (className =? "SpeedCrunch") (placeWindow 0.05 0.05 0.55 0.55)
    , NS "calendar"     "$HOME/.scripts/launch-google-calendar-app" (fmap ("calendar.google.com" `isInfixOf`) appName) (rightPanel 0.02)
    -- , NS "ghci"       "urxvtc -e ghci" (title =? "ghci") (centerScreen 0.7 0.7)
    ]

------------------------------------------------------------------------
-- Main thing
------------------------------------------------------------------------

main = do

    xmobarPipe <- spawnPipe "xmobar --position='TopP 0 0' /home/hives/.xmonad/xmobar.conf"

    xmonad
        $ withUrgencyHook NoUrgencyHook
        $ ewmh
        $ addDescrKeys' ((myModMask, xK_F1), showKeybindings) myKeys2
        $ myConfig xmobarPipe

myConfig p = def
      { borderWidth        = border
      , clickJustFocuses   = myClickJustFocuses
      , focusFollowsMouse  = myFocusFollowsMouse
      , focusedBorderColor = cActive
      , handleEventHook    = myHandleEventHook
      , layoutHook         = myLayoutHook
      , logHook            = myXmobarLogHook p
                         <+> fadeInactiveLogHook 0.95
      , manageHook         = myManageHook
                         <+> manageHook defaultConfig
      , modMask            = myModMask
      , mouseBindings      = myMouseBindings
      , normalBorderColor  = cVisible
      -- , startupHook        = myStartupHook
      , terminal           = myTerminal
      , XMonad.workspaces  = myWorkspaces
      }

---------------------------------------------------------------------------
-- X Event Actions
---------------------------------------------------------------------------

myHandleEventHook  = docksEventHook -- Whenever a new dock appears, refresh the layout immediately to avoid the new dock.
                 <+> handleEventHook defaultConfig
                 -- <+> XMonad.Hooks.EwmhDesktops.fullscreenEventHook -- Enable fullscreen in ewmh applications
                 <+> XMonad.Layout.Fullscreen.fullscreenEventHook

------------------------------------------------------------------------
-- ManageHook stuff
------------------------------------------------------------------------

myManageHook = composeAll
    [ namedScratchpadManageHook scratchpads

    -- , isFullscreen --> doFullFloat

    -- excludes statusbars from tiling space (i think?)
    , manageDocks

    -- Float images
    , appName =? "feh" --> doCenterFloat

    -- Send applications to workspaces
    , appName =? "preview.web.skype.com__en" --> doShift "1" <+> unfloat
    , appName =? "skype" --> doShift "1" <+> unfloat
    , appName =? "jellymould-group.slack.com__messages" --> doShift "1" <+> unfloat
    , appName =? "soulseekqt" --> doShift "2"
    , appName =? "transmission-gtk" --> doShift "6"
    , stringProperty "WM_WINDOW_ROLE" =? "browser" --> doShift "7"
    , fmap ("Sublime Text (UNREGISTERED)" `isInfixOf`) title --> doShift "9"

    -- Pop up help pages
    , title =? "Paul's Special Less" --> centerScreen 0.5 0.9
    , (className =? "feh" <&&> fmap ("GuiFN layer" `isInfixOf`) title) --> doCenterFloat
    -- , (className =? "Zathura" <&&> fmap ("ErgoDox\\ EZ\\ Configurator.pdf" `isInfixOf`) title) --> doCenterFloat
    -- , (className =? "Zathura" <&&> fmap ("Er" `isInfixOf`) title) --> doCenterFloat
    -- , (className =? "Zathura") --> doCenterFloat

    -- Pop ups
    , (className =? "Chromium" <&&> stringProperty "WM_WINDOW_ROLE" =? "pop-up") --> doCenterFloat
    , (stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog") --> doCenterFloat
    , (title =? "Open" <&&> className =? "MComix") --> doCenterFloat

    -- Email
    , (className =? "Thunderbird" <&&> stringProperty "WM_WINDOW_ROLE" =? "3pane") --> doShift "0"
    , appName =? "Msgcompose" --> centerScreen 0.61 0.7
    , title =? "Thunderbird Preferences" --> centerScreen 0.5 0.4
    , appName =? "mailspring" --> doShift "0"
    , title =? "Unlock Login Keyring" --> doShift "0" <+> doCenterFloat

    -- GIMP
    , stringProperty "WM_WINDOW_ROLE" =? "gimp-message-dialog" --> centerScreen 0.3 0.3
    , stringProperty "WM_WINDOW_ROLE" =? "gimp-toolbox-color-dialog" --> centerScreen 0.4 0.4

    ]
    where unfloat = ask >>= doF . W.sink

------------------------------------------------------------------------
-- Status bar stuff
------------------------------------------------------------------------

myXmobarLogHook :: Handle -> X ()
myXmobarLogHook pipe =
    dynamicLogWithPP xmobarPP
        { ppOutput          = hPutStrLn pipe
        , ppOrder           = \(ws:l:t:_)   -> [ws,l,t]
        , ppCurrent         = xmobarColor cBackground cActive . wrap " " " " . noScratchPad
        , ppVisible         = xmobarColor cVisibleWorkspaceText cVisibleWorkspaceBackground . wrap " " " " . noScratchPad
        , ppTitle           = xmobarColor cActive "" . pad . shorten 50
        , ppHidden          = xmobarColor "#5b605e" "" . pad . noScratchPad
        -- , ppHidden          = check -- see https://github.com/altercation/dotfiles-tilingwm/blob/master/.xmonad/xmonad.hs
        , ppHiddenNoWindows = const ""
        , ppLayout          = xmobarColor cText "" . wrap " " ""
        , ppUrgent          = xmobarColor cBackground cUrgent . pad
        , ppSep             = " "
        , ppWsSep           = ""
        }
    where
        noScratchPad ws = if ws == "NSP" then "" else ws

------------------------------------------------------------------------
-- Workspace stuff
------------------------------------------------------------------------

-- myWorkspaces :: [String] 
myWorkspaces = ["1","2","3","4","5","6","7","8","9","0"]
  
------------------------------------------------------------------------
-- Layout stuff
------------------------------------------------------------------------

myLayoutHook = smartBorders
             $ showWorkspaceName
             $ fullScreenToggle
             $ hiddenWindows
             $ avoidStruts
             $ dwindle ||| tabs ||| tall
             -- $ tall ||| tabs
    where

        showWorkspaceName = showWName' myShowWNameTheme

        dwindle = named "Dwindle"
             -- $ addTopBar
             $ spacingWithEdge gutter
             $ mkToggle (single MIRROR)
             $ Dwindle R XMonad.Layout.Dwindle.CW 1.5 1.1

        tall = named "Tall"
             -- $ addTopBar
             $ spacingWithEdge gutter
             $ mkToggle (single MIRROR)
             $ Tall 1 (2/100) (1/2)

        tabs = named "Tabs"
             $ addTabs shrinkText myTabTheme
             $ spacingWithEdge gutter
             $ Simplest

        -- addTopBar = noFrillsDeco shrinkText topBarTheme

        fullScreenToggle = mkToggle (single FULL)

        -- tall = named "Tall"
        --      -- this removes borders on fullscreen floated windows
        --      $ lessBorders OnlyFloat

        --      $ avoidStruts

        --      -- this is necessary because we need 2D navigation to create sublayouts or something
        --      $ configurableNavigation (navigateColor cDeselected)
        --      $ addTopBar
        --      $ addTabs shrinkText myTabTheme
        --      $ subLayout [] (Simplest ||| Accordion)
        --      -- $ subLayout [] (tabbed shrinkText myTabTheme ||| Accordion)
        --      $ spacingWithEdge gutter
        --      $ Tall 1 (2/100) (1/2)

        -- imLayout = withIM (1/10) (Role "buddy_list") standardLayouts
        -- imLayout = withIM (1%6) pidginRoster Grid
        -- pidginRoster = And (ClassName "Pidgin") (Role "buddy_list")

------------------------------------------------------------------------
-- Key bindings
------------------------------------------------------------------------

myModMask = mod4Mask

wsKeys = map show $ [1..9] ++ [0]

-- Display keyboard mappings using less.
-- Modified from https://github.com/thomasf/dotfiles-thomasf-xmonad/blob/master/.xmonad/lib/XMonad/Config/A00001.hs
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
    h <- spawnPipe "$HOME/.scripts/less-standalone"
    hPutStr h (unlines $ showKm x)
    hClose h
    return ()

shiftAndView dir = findWorkspace getSortByIndexNoSP dir HiddenWS 1
        >>= \t -> (windows . W.shift $ t) >> (windows . W.greedyView $ t)
nextNonEmptyWS = findWorkspace getSortByIndexNoSP Next HiddenNonEmptyWS 1
        >>= \t -> (windows . W.view $ t)
prevNonEmptyWS = findWorkspace getSortByIndexNoSP Prev HiddenNonEmptyWS 1
        >>= \t -> (windows . W.view $ t)
getSortByIndexNoSP =
        fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex

myKeys2 conf = let

    subKeys str ks = subtitle str : mkNamedKeymap conf ks

    zipM  m nm ks as f = zipWith (\k d -> (m ++ k, addName nm $ f d)) ks as
    zipM' m nm ks as f b = zipWith (\k d -> (m ++ k, addName nm $ f d b)) ks as

    -- try sending one message, fallback if unreceived, then refresh
    -- tryMsgR x y = sequence_ [(tryMessage_ x y), refresh]
    tryMsgR x y = sequence_ [(tryMessageWithNoRefreshToCurrent x y), refresh]

    toggleFloat w = windows (\s -> if M.member w (W.floating s)
                    then W.sink w s
                    else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s))

    in

    -----------------------------------------------------------------------
    -- System / Utilities
    -----------------------------------------------------------------------

    subKeys "System"
    [ ("M-q"                     , addName "Restart XMonad"                      $ spawn "xmonad --restart")
    , ("M-C-q"                   , addName "Rebuild & restart XMonad"            $ spawn "xmonad --recompile && xmonad --restart")
    , ("M-S-q"                   , addName "Quit XMonad"                         $ confirmPrompt hotPromptTheme "Quit XMonad" $ io (exitWith ExitSuccess))
    , ("M-S-C-q"                 , addName "Shutdown"                            $ confirmPrompt hotPromptTheme "Shutdown" $ spawn "shutdown now")
    , ("M-x"                     , addName "Lock screen"                         $ spawn "xscreensaver-command -lock")
    ] ^++^

    -----------------------------------------------------------------------
    -- Help
    -----------------------------------------------------------------------

    subKeys "Help"
    [ ("M-F1"                    , addName "Show Keybindings"                    $ return ())
    -- , ("M-b"                     , addName "Show bash shortcuts"                 $ spawn ("$HOME/.scripts/less-standalone $HOME/Documents/my\\ help\\ files/bash-keys.txt"))
    , ("M-o"                     , addName "Show orgmode bindings"               $ spawn ("$HOME/.scripts/less-standalone $HOME/Documents/my\\ help\\ files/org-mode-keys.txt"))
    -- , ("M-p"                     , addName "Show pseudo Fn layer"                $ spawn ("feh $HOME/Documents/my\\ help\\ files/GuiFN\\ layer.png"))
    -- , ("M-e"                     , addName "Show Ergodox layout"                 $ spawn ("zathura $HOME/Downloads/ErgoDox\\ EZ\\ Configurator\\ COLEMAK.pdf"))
    , ("M-e"                     , addName "Show Ergodox bindings"               $ spawn ("$HOME/.scripts/less-standalone $HOME/Documents/my\\ help\\ files/ergodox-keys.txt"))
    ] ^++^

    -----------------------------------------------------------------------
    -- Windows
    -----------------------------------------------------------------------

    subKeys "Windows"
    [ ("M-<Backspace>"           , addName "Kill"                                kill1)
    , ("M-<Delete>"              , addName "Kill"                                kill1)
    , ("M-S-<Backspace>"         , addName "Kill all"                            $ confirmPrompt hotPromptTheme "kill all" $ killAll)
    , ("M-S-<Delete>"            , addName "Kill all"                            $ confirmPrompt hotPromptTheme "kill all" $ killAll)

    , ("M-d"                     , addName "Duplicate w to all ws"               $ toggleCopyToAll)


    , ("M-/"                     , addName "Hide window"                         $ (withFocused hideWindow))
    , ("M-S-/"                   , addName "Restore hidden window"               $ (popNewestHiddenWindow))

    , ("M-<Return>"              , addName "Promote to master pane"              $ promote) 

    -- QWERTY controls
    , ("M-j"                     , addName "Focus down"                          $ windows W.focusDown)
    , ("M-k"                     , addName "Focus up"                            $ windows W.focusUp)
    , ("M-S-j"                   , addName "Swap down"                           $ windows W.swapDown)
    , ("M-S-k"                   , addName "Swap up"                             $ windows W.swapUp)

    -- COLEMAK controls
    -- , ("M-n"                     , addName "Focus down"                          $ windows W.focusDown)
    -- , ("M-e"                     , addName "Focus up"                            $ windows W.focusUp)
    -- , ("M-S-n"                   , addName "Swap down"                           $ windows W.swapDown)
    -- , ("M-S-e"                   , addName "Swap up"                             $ windows W.swapUp)

    , ("M-t"                     , addName "Unfloat window"                      $ withFocused $ windows . W.sink)

    ] ^++^

    -----------------------------------------------------------------------
    -- Workspaces
    -----------------------------------------------------------------------

    subKeys "Workspaces"
    (
    [ ("M1-<Tab>"                , addName "Toggle last workspace"               $ toggleWS' ["NSP"])
    , ("M-<Page_Down>"           , addName "Next non-empty workspace"            $ nextNonEmptyWS)
    , ("M-<Page_Up>"             , addName "Prev non-empty workspace"            $ prevNonEmptyWS)
    , ("M-S-<Page_Down>"         , addName "Move w to next workspace"            $ shiftAndView Next)
    , ("M-S-<Page_Up>"           , addName "Move w to next workspace"            $ shiftAndView Prev)
    ]
    ++ zipM "M-"                 "View      ws"                                  wsKeys [0..] (withNthWorkspace W.greedyView)
    ++ zipM "M-S-"               "Move w to ws"                                  wsKeys [0..] (withNthWorkspace W.shift)
    ++ zipM "M-S-C-"             "Copy w to ws"                                  wsKeys [0..] (withNthWorkspace copy)
    ) ^++^

    -----------------------------------------------------------------------
    -- Screens
    -----------------------------------------------------------------------

    subKeys "Screens"
    [ ("M-s"                     , addName "Focus on next screen"                $ nextScreen)
    , ("M-S-s"                   , addName "Move w to next screen"               $ shiftNextScreen >> nextScreen)

    , ("M-<Left>"                , addName "Focus on primary screen"             $ viewScreen def 0)
    , ("M-<Right>"               , addName "Focus on secondary screen"           $ viewScreen def 1)
    , ("M-S-<Left>"              , addName "Move w to primary screen"            $ sendToScreen def 0 >> viewScreen def 0)
    , ("M-S-<Right>"             , addName "Move w to secondary screen"          $ sendToScreen def 1 >> viewScreen def 1)
    ] ^++^


    -----------------------------------------------------------------------
    -- Layouts
    -----------------------------------------------------------------------

    subKeys "Layouts"
    [ ("M-<Tab>"                 , addName "Cycle all layouts"                   $ sendMessage NextLayout)
    , ("M-S-<Tab>"               , addName "Reset layout"                        $ setLayout $ XMonad.layoutHook conf)

    , ("M-l"                     , addName "Expand master pane"                  $ sendMessage (Expand))
    , ("M-h"                     , addName "Shrink master pane"                  $ sendMessage (Shrink))

    , ("M-,"                     , addName "Increase master windows"             $ sendMessage (IncMasterN 1))
    , ("M-."                     , addName "Decrease master windows"             $ sendMessage (IncMasterN (-1)))

    , ("M-\\"                    , addName "Mirror layout"                       $ sendMessage $ Toggle MIRROR)

    , ("M-S-="                     , addName "Decrease window spacing"             $ decScreenWindowSpacing 2)
    , ("M--"                     , addName "Increase window spacing"             $ incScreenWindowSpacing 2)
    , ("M-w"                     , addName "Reset window spacing"                $ setScreenWindowSpacing 6)

    -- If following is run on a floating window, the sequence first tiles it.
    -- Not perfect, but works.
    , ("M-f"                     , addName "Fullscreen"                          $ sequence_ [ (withFocused $ windows . W.sink)
                                                                                    , (sendMessage $ XMonad.Layout.MultiToggle.Toggle FULL) ])

    -- Fake fullscreen fullscreens into the window rect. The expand/shrink
    -- is a hack to make the full screen paint into the rect properly.
    -- The tryMsgR handles the BSP vs standard resizing functions.
    , ("M-S-f"                   , addName "Fake fullscreen"                     $ sequence_ [ (P.sendKey P.noModMask xK_F11)
                                                                                    , (tryMsgR (ExpandTowards L) (Shrink))
                                                                                    , (tryMsgR (ExpandTowards R) (Expand)) ])
    ] ^++^

    -----------------------------------------------------------------------
    -- Launchers
    -----------------------------------------------------------------------

    subKeys "Launchers"
    [ ("M-<Space>"               , addName "Launcher"                            $ spawn myLauncher)
    , ("M-S-<Return>"            , addName "Terminal"                            $ spawn myTerminal)
    , ("M-r"                     , addName "File browser"                        $ spawn myFileBrowser)
    , ("<Print>"                 , addName "Copy screengrab to clipboard"        $ spawn ("$HOME/.scripts/screengrab-copy"))
    , ("S-<Print>"               , addName "Save screengrab"                     $ spawn ("$HOME/.scripts/screengrab-save"))
    , ("M1-<Print>"              , addName "Save screengrab of active window"    $ spawn ("$HOME/.scripts/screengrab-save-window"))
    , ("M1-C-<Return>"           , addName "NSP terminal"                        $ namedScratchpadAction scratchpads "terminal")
    -- , ("M1-C-d"                  , addName "NSP terminal (~/Downloads)"          $ namedScratchpadAction scratchpads "downloads")
    , ("M-m"                     , addName "NSP music player"                    $ namedScratchpadAction scratchpads "musicplayer")
    , ("<XF86Tools>"             , addName "NSP music player"                    $ namedScratchpadAction scratchpads "musicplayer")
    , ("M-c"                     , addName "NSP Google"                          $ namedScratchpadAction scratchpads "calendar")
    , ("M-S-h"                   , addName "NSP htop"                            $ namedScratchpadAction scratchpads "htop")
    , ("M-v"                     , addName "NSP alsamixer"                       $ namedScratchpadAction scratchpads "alsamixer")
    , ("<XF86Calculator>"        , addName "NSP calculator"                      $ namedScratchpadAction scratchpads "calculator")

    , ("M-C-S-j"                 , addName "Start Jelly apps"                    $ sequence_ [ spawn jellyBrowser
                                                                                             , spawn jellyNotes
                                                                                             , spawn jellySkype
                                                                                             , spawn jellySlack
                                                                                             , spawn jellyEmail
                                                                                             , viewScreen def 1
                                                                                             , windows $ W.greedyView "1"
                                                                                             , setLayout $ XMonad.layoutHook conf
                                                                                             , sendMessage (Shrink)
                                                                                             , sendMessage (Shrink)
                                                                                             , sendMessage (Shrink)
                                                                                             , sendMessage (Shrink)
                                                                                             , viewScreen def 0
                                                                                             , windows $ W.greedyView "7"
                                                                                             , setLayout $ XMonad.layoutHook conf
                                                                                             , sendMessage (Expand)
                                                                                             , sendMessage (Expand)
                                                                                             , sendMessage (Expand)
                                                                                             , sendMessage (Expand)
                                                                                             , sendMessage (Expand)
    --                                                                                         , spawn "xmonad --restart"
                                                                                             ])

    ] ^++^

    -----------------------------------------------------------------------
    -- Appearance
    -----------------------------------------------------------------------

    subKeys "Appearance"
    [ ("M-S-t"                   , addName "Toggle light/dark colour scheme"     $ spawn "$HOME/.scripts/toggle-colours")
    ] ^++^

    -----------------------------------------------------------------------
    -- Media controls
    -----------------------------------------------------------------------

    subKeys "Media controls"
    [ ("<XF86AudioRaiseVolume>"  , addName "Volume +2%"                          $ spawn "$HOME/.scripts/vol -c up -i 2")
    , ("<XF86AudioLowerVolume>"  , addName "Volume -2%"                          $ spawn "$HOME/.scripts/vol -c down -i 2")
    , ("S-<XF86AudioRaiseVolume>", addName "Volume +10%"                         $ spawn "$HOME/.scripts/vol -c up -i 10")
    , ("S-<XF86AudioLowerVolume>", addName "Volume -10%"                         $ spawn "$HOME/.scripts/vol -c down -i 10")
    , ("<XF86AudioMute>"         , addName "Toggle volume"                       $ spawn "$HOME/.scripts/vol -c mute")

    -- , ("<XF86AudioPlay>"         , addName "Play/pause"                          $ spawn "mpc toggle")
    -- -- , ("<XF86AudioNext>"         , addName "Next track"                          $ spawn "mpc next")
    -- -- , ("<XF86AudioPrev>"         , addName "Previous track"                      $ spawn "mpc prev")
    -- , ("<XF86AudioNext>"         , addName "Seek forwards 10s"                   $ spawn "mpc seek +00:00:05")
    -- , ("<XF86AudioPrev>"         , addName "Seek backwards 10s"                  $ spawn "mpc seek -00:00:05")
    -- , ("<XF86AudioStop>"         , addName "Stop music"                          $ spawn "mpc stop")

    , ("<XF86AudioPlay>"         , addName "Play/pause"                         $ spawn "mocp -G")
    , ("<XF86AudioNext>"         , addName "Next track"                         $ spawn "mocp -f")
    , ("<XF86AudioPrev>"         , addName "Previous track"                     $ spawn "mocp -r")
    , ("<XF86AudioStop>"         , addName "Stop music"                         $ spawn "mocp -s")
    ]

    where
        toggleCopyToAll = wsContainingCopies >>= \ws -> case ws of
          [] -> windows copyToAll
          _ -> killAllOtherCopies

-- Mouse bindings: default actions bound to mouse events
-- Includes window snapping on move/resize using X.A.FloatSnap
-- Includes window w/h ratio constraint (square) using X.H.ConstrainedResize
myMouseBindings (XConfig {XMonad.modMask = myModMask}) = M.fromList $

    [ ((myModMask,               button1) ,(\w -> focus w
      >> mouseMoveWindow w
      >> ifClick (snapMagicMove (Just 50) (Just 50) w)
      >> windows W.shiftMaster))

    , ((myModMask .|. shiftMask, button1), (\w -> focus w
      >> mouseMoveWindow w
      >> ifClick (snapMagicResize [L,R,U,D] (Just 50) (Just 50) w)
      >> windows W.shiftMaster))

    , ((myModMask,               button3), (\w -> focus w
      >> mouseResizeWindow w
      >> ifClick (snapMagicResize [R,D] (Just 50) (Just 50) w)
      >> windows W.shiftMaster))

    , ((myModMask .|. shiftMask, button3), (\w -> focus w
      >> Sqr.mouseResizeWindow w True
      >> ifClick (snapMagicResize [R,D] (Just 50) (Just 50) w)
      >> windows W.shiftMaster ))

--    , ((mySecondaryModMask,      button4), (\w -> focus w
--      >> prevNonEmptyWS))
--
--    , ((mySecondaryModMask,      button5), (\w -> focus w
--      >> nextNonEmptyWS))

    ]


------------------------------------------------------------------------
-- Utils
------------------------------------------------------------------------

-- | Calculate center of screen rectangle
centerScreen :: Rational -> Rational -> ManageHook
centerScreen w h = doRectFloat $ W.RationalRect ((1 - w)/2) ((1 - h)/2) w h

-- | Place window on screen. Parameters are margins, clockwise from top
placeWindow :: Rational -> Rational -> Rational -> Rational -> ManageHook
placeWindow t r b l = doRectFloat $ W.RationalRect l t (1-r-l) (1-t-b)

-- | Place a window over one half of the screen. Parameter is the margin or something
rightPanel :: Rational -> ManageHook
rightPanel b = placeWindow (b+0.012) b b (0.5+b) 
leftPanel :: Rational -> ManageHook
leftPanel b = placeWindow (b+0.012) (0.5+b) b b 

