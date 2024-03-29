module Shared.PageState exposing
    ( PageState
    , mapPageState
    , setPageState
    , bimapPsCmd
    )

import Time as Time
import Browser.Navigation as Nav
import Url exposing (Url)

type alias PageState navbarState a =
    { key : Nav.Key
    , timeZone : Time.Zone
    , currentTime : Time.Posix
    , state : a
    , pageUrl : Url
    , navbarState : navbarState
    }

mapPageState : (a -> b) -> PageState navbarState a -> PageState navbarState b
mapPageState f ps =
    { state = f ps.state
    , timeZone = ps.timeZone
    , currentTime = ps.currentTime
    , key = ps.key
    , pageUrl = ps.pageUrl
    , navbarState = ps.navbarState
    }

setPageState : a -> PageState navbarState b -> PageState navbarState a
setPageState = mapPageState << always

bimapPsCmd : (s1 -> s2) -> (cmd1 -> cmd2) -> (PageState navbarState s1, Cmd cmd1) -> (PageState navbarState s2, Cmd cmd2)
bimapPsCmd f g (pageState, cmd) = (mapPageState f pageState, Cmd.map g cmd)
