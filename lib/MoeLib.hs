module MoeLib ( moeStatusBar) where

import XMonad
import XMonad.Util.Run
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import Keys(moeKeysP, moeKeysAlter)
import qualified Data.Map as M
import Control.Monad (liftM2)
-- import XMonad.Layout.LayoutModifier

moeLogPP = defaultPP
    { ppCurrent = xmobarColor "#000000" "#f17413" . wrap " " " " . xmobarStrip
    , ppHidden  = xmobarColor "#C0C0C0" "#6A5ACD" . wrap " " " " . xmobarStrip
    , ppHiddenNoWindows = xmobarColor "#c0c0c0" ""
    , ppUrgent  = xmobarColor "#FFD700" "#DC143C" . wrap " " " " . xmobarStrip
    , ppVisible = wrap "(" ")"
    , ppTitle   = xmobarColor "#00FF00" "" . shorten 40
    , ppSep     = " :: "
    , ppWsSep   = " "
    }

moeStatusBar cmd pp k isLog conf = do
    h <- spawnPipe cmd
    return $ conf
        { layoutHook = avoidStruts (layoutHook conf)
        , logHook    = statusLogHook isLog h
        , manageHook = manageHook conf <+> manageDocks
        , keys       = liftM2 M.union keys' (keys conf)
        }
        where
            keys' = (`M.singleton` sendMessage ToggleStruts) . k
            statusLogHook isHook h
                | True = do
                    logHook conf
                    dynamicLogWithPP pp { ppOutput = hPutStrLn h }
                | False = logHook conf

