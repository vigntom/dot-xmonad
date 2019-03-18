module Keys where

import           Data.Char                          (toLower)
import           Data.List                          (isInfixOf)
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.LayoutCombinators
import           XMonad.Prompt
import           XMonad.Prompt.Shell
import           XMonad.Prompt.XMonad
import qualified XMonad.StackSet                    as W

--
-- # Configuration for shellPrompt
--
-- ## 1. dmenu like
dmenuLikeXPConfig :: XPConfig
dmenuLikeXPConfig =
  def
  { font = "xft:Terminus-10"
  , bgColor = "#1C1C1C"
  , fgColor = "#92cced"
  , bgHLight = "#f17413"
  , fgHLight = "#18001B"
  , position = Top
  , alwaysHighlight = True
  , searchPredicate = isInfixOf . map toLower
  }

-- ## 2. xmobar like
xmobarLikeXPConfig :: XPConfig
xmobarLikeXPConfig =
  dmenuLikeXPConfig {bgColor = "#1C1C1C", fgColor = "#ca8f2d"}

moeXPConfig :: XPConfig
moeXPConfig =
  dmenuLikeXPConfig
  {bgColor = "#1C1C1C", fgColor = "#AFD700", borderColor = "#005F5F"}

restartXmonad :: X ()
restartXmonad = do
  spawn "killall xmobar"
  spawn "killall lxpanel"
  spawn
    ("if type xmonad;" `mappend` "then xmonad --recompile && xmonad --restart;" `mappend`
     "else xmessage xmonad not in \\$PATH: \"$PATH\";" `mappend`
     "fi")

curLayout :: X String
curLayout =
  fmap (description . W.layout . W.workspace . W.current) (gets windowset)

defaultLayoutKeys :: [(String, X ())]
defaultLayoutKeys =
  [ ("M-S-h", sendMessage Shrink)
  , ("M-S-<Up>", sendMessage $ JumpToLayout "Full")
  , ("M-S-f", sendMessage $ JumpToLayout "Maximazed")
  , ("M-S-\\", sendMessage $ JumpToLayout "Tall")
  , ("M-S-/", sendMessage $ JumpToLayout "Mirror")
  , ("M-S-b", sendMessage $ JumpToLayout "BSP")
  , ("M-S-l", sendMessage Expand)
  , ("M-M1-<Space>", sendMessage NextLayout)
  ]

bspLayoutKeys :: [(String, X ())]
bspLayoutKeys =
  [ ("M-M1-l", sendMessage $ ExpandTowards R)
  , ("M-M1-h", sendMessage $ ExpandTowards L)
  , ("M-M1-j", sendMessage $ ExpandTowards D)
  , ("M-M1-k", sendMessage $ ExpandTowards U)
  , ("M-M1-C-l", sendMessage $ ShrinkFrom R)
  , ("M-M1-C-h", sendMessage $ ShrinkFrom L)
  , ("M-M1-C-j", sendMessage $ ShrinkFrom D)
  , ("M-M1-C-k", sendMessage $ ShrinkFrom U)
  , ("M-r", sendMessage Rotate)
  , ("M-s", sendMessage Swap)
  , ("M-n", sendMessage FocusParent)
  , ("M-C-n", sendMessage SelectNode)
  , ("M-S-n", sendMessage MoveNode)
  , ("M-a", sendMessage Balance)
  , ("M-S-a", sendMessage Equalize)
  ]

moeKeysP :: XConfig a -> [(String, X ())]
moeKeysP conf =
  [ ("M-p", shellPrompt moeXPConfig)
  , ("M-S-;", xmonadPrompt dmenuLikeXPConfig {position = Bottom})
  , ("M-q", restartXmonad)
  , ("M-h", windows W.focusUp)
  , ("M-l", windows W.focusDown)
  , ("M-S-c", kill)
  , ("M-C-s", spawn "xscreensaver-command -lock")
  , ("M-b", sendMessage ToggleStruts)
  , ("M-z", toggleWS)
  ] `mappend`
  defaultLayoutKeys `mappend`
  bspLayoutKeys `mappend`
  [ (otherModMask `mappend` "M-" `mappend` key, action tag)
  | (tag, key) <-
      zip
        (XMonad.workspaces conf)
        [ "`"
        , "1"
        , "2"
        , "3"
        , "4"
        , "5"
        , "6"
        , "7"
        , "8"
        , "9"
        , "0"
        , "-"
        , "="
        , "<Backspace>"
        ]
  , (otherModMask, action) <-
      [("", windows . W.greedyView), ("S-", windows . W.shift)]
  ]

moeUnboundKeys :: [String]
moeUnboundKeys = ["M-<Space>", "M-S-<Space>", "M-c", "M-v"]
