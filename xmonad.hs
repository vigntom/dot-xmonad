import Data.Functor
import XMonad hiding ( (|||) )
import XMonad.Layout hiding ( (|||) )
import XMonad.Layout.Renamed
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Fullscreen
import XMonad.Util.Run
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import XMonad.Layout.NoBorders
import XMonad.Actions.UpdateFocus
import qualified XMonad.StackSet as W
import Control.Monad (liftM2)
import Keys(moeKeysP, moeUnboundKeys)
import XMonad.Util.EZConfig
import XMonad.Util.NamedWindows (getName)
import Data.List
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.BinarySpacePartition
import GHC.IO.Handle

moeModMask :: KeyMask
moeModMask = mod4Mask

moeTerminal :: String
moeTerminal = "alacritty"  -- termite urxvt"

moeWorkSpaces :: [String]
moeWorkSpaces = [ "~:a"
                , "1:b", "2:c", "3:d", "4:e", "5:f"
                , "6:g", "7:h", "8:i", "9:j", "0:k"
                , "-:l", "=:m", "<:n"
                ]

-- use xprop for detecting window properties
-- command: xprop | grep WM_CLASS (click on window)
-- result:  WM_CLASS(STRING) = "volti-mixer", "Volti-mixer"
--
-- values:  appName = "volti-mixer", className = "Volti-mixer"

moeManageHook :: ManageHook
moeManageHook = composeAll . concat $
   [ [ isClass "Firefox"                       --> viewShift "3:d"]
   , [ isClassAndTitle"Inkscape" "Effect"      --> doFloat ]
   , [ isClass "Firefox"
       <&&> isIn resource "Dialog"             --> doFloat]
   , [ isTitle t --> doFloat | t <- myTitleFloats]
   , [ isClass c --> doFloat | c <- myClassFloats]
   ]
   where
       myTitleFloats = ["Pong"]
       myClassFloats = [ "XFontSel"
                       , "Gksu"
                       , "Volti"
                       , "Volti-mixer"
                       , "Virt-viewer"
                       , "Pong"
                       , "Deadbeef" ]
       viewShift = doF . liftM2 (.) W.greedyView W.shift
       isIn entity name = isInfixOf name <$> entity
       isClass = isIn className
       isTitle = isIn title
       isClassAndTitle c t = isClass c <&&> isTitle t

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name     <- getName w
    Just idx <-  W.findTag w <$> gets windowset
    safeSpawn "notify-send" [show name, mappend "workspace " idx]

moeLayout = avoidStruts (uncurry named bsp ||| uncurry named maxi) ||| uncurry named full
    where named s = renamed [Replace s]
          full    = ("Full", noBorders Full)
          maxi    = ("Maximazed", noBorders $ fullscreenFull Full)
          bsp     = ("BSP", smartBorders emptyBSP)

moeLogPP :: PP
moeLogPP = def
    { ppCurrent = xmobarColor "#000000" "#f17413" . wrap " " " " . xmobarStrip
    , ppHidden  = xmobarColor "#C0C0C0" "#6A5ACD" . wrap " " " " . xmobarStrip
    , ppUrgent  = xmobarColor "#FFD700" "#DC143C" . wrap " " " " . xmobarStrip
    , ppVisible = wrap "(" ")"
    , ppTitle   = xmobarColor "#00FF00" "" . shorten 33
    , ppSep     = "::"
    , ppWsSep   = " "
    }

moeStatusTopCall :: String
moeStatusTopCall    = "xmobar ~/.xmonad/xmobar.rc"

moeStatusBottomCall :: String
moeStatusBottomCall = "xmobar ~/.xmonad/xmobar-bottom.rc"

moeLxPanel :: String
moeLxPanel = "lxpanel"

moeStartupHook :: X ()
moeStartupHook = do
  ewmhDesktopsStartup
  setWMName "LG3D"
  adjustEventInput
  checkKeymap moeDefaultConfig $ moeKeysP moeDefaultConfig

main :: IO ()
main = do
    other  <- spawnPipe moeLxPanel
    top    <- spawnPipe moeStatusTopCall
    bottom <- spawnPipe moeStatusBottomCall
    xmonad $ moeConfig top

moeConfig bar = ewmh moeDefaultConfig
                { logHook = logHook def <+> dynamicLogWithPP moeLogPP { ppOutput = hPutStrLn bar }}

moeDefaultConfig = fullscreenSupport
  $ withUrgencyHook LibNotifyUrgencyHook
  $ def { terminal        = moeTerminal
        , modMask         = moeModMask
        , workspaces      = moeWorkSpaces
        , startupHook     = startupHook def <+> moeStartupHook
        , handleEventHook = handleEventHook def <+> focusOnMouseMove <+> docksEventHook <+> fullscreenEventHook
        , manageHook      = manageHook def <+> moeManageHook <+>  manageDocks <+> fullscreenManageHook
        , layoutHook      = moeLayout
        }
        `additionalKeysP` moeKeysP moeDefaultConfig
        `removeKeysP` moeUnboundKeys

