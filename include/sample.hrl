-ifndef(SAMPLE_HRL).
-define(SAMPLE_HRL, true).

-type starType()      :: star_add | star_remove.
-type roomType()      :: group | channel.
-type linkStatus()    :: lgen | lcheck | ladd | ldelete | lupdate.
-type presence()      :: online | offline.
-type profileStatus() :: remove_profile | get_profile | patch_profile.
-type memberStatus()  :: admin | member | removed | patch | owner.
-type container()     :: chain | cur.
-type tagType()       :: tag_create | tag_remove | tag_edit.
-type messageType()   :: sys | reply | forward | read | edited | cursor.
-type messageStatus() :: masync | mdelete | mclear| mupdate | medit.
-type authOs()        :: ios | android | web.
-type serverType()    :: email | wallet | google_type | fb | phone.
-type serverStatus()  :: servie_verified | service_not_verified.
-type roomStatus()    :: room_create | room_leave| room_add | room_remove | room_patch |
                         room_get | room_delete | room_last_msg.
-type authType()      :: google_auth | facebook_auth | mobile_auth | email_auth |
                         voice | resend | verify | push | logout | get | delete | clear.
-type rosterStatus()  :: get_roster | create_roster | del_roster | remove_roster| nick| add_roster | update_roster |
                         list_loster | patch_roster | roster_last_msg.
-type contactStatus() :: conact_request | authorization | contact_ignore | conatct_internal |
                         friend | contact_last_msg | contact_ban | conact_banned | contact_deleted.
-type authStatus()    :: invalid_version | mismatch_user_data | number_not_allowed |
                         session_not_found | attempts_expired | invalid_sms_code |
                         invalid_jwt_code | permission_denied | invalid_data.

-record(muc,            {name = [] :: [] | binary() }).
-record(p2p,            {from = [] :: [] | binary(),
                         to   = [] :: [] | binary() }).

-record('Feature',      {id    = [] :: [] | binary(),
                         key   = [] :: [] | binary(),
                         value = [] :: [] | binary(),
                         group = [] :: [] | binary()}).

-record('Desc',         {id       = [] :: binary(),
                         mime     = <<"text">> :: binary(),
                         payload  = [] :: binary(),
                         parentid = [] :: binary(),
                         data     = [] :: list(#'Feature'{})}).

-record('Tag',          {roster_id = [] :: [] | integer(),
                         name      = [] :: binary(),
                         color     = [] :: binary(),
                         tag_status    = [] :: tagType()}).

-record('Message',      {id        = [] :: [] | integer(),
                         container = chain :: container(),
                         feed_id   = [] :: #muc{} | #p2p{},
                         prev      = [] :: [] | integer(),
                         next      = [] :: [] | integer(),
                         msg_id    = [] :: [] | binary(),
                         from      = [] :: [] | binary(),
                         to        = [] :: [] | binary(),
                         created   = [] :: [] | integer(),
                         files     = [] :: list(#'Desc'{}),
                         type      = [] :: messageType(),
                         link      = [] :: [] | integer() | #'Message'{},
                         seenby    = [] :: list(binary() | integer()),
                         repliedby = [] :: list(integer()),
                         mentioned = [] :: list(integer()),
                         msg_status    = [] :: messageStatus()}).

-define(CUR,  id =  [] :: term(),
              top=  [] :: [] | integer(),
              bot=  [] :: [] | integer(),
              dir=   0 ::  0 | 1,
              reader=  [] :: [] | tuple(),
              writer=  [] :: [] | tuple()).
-record(cur,  {?CUR, left=0, right=0, args=[]::list(tuple()|integer()), money=0, status=[]}).

-record('Auth',         {client_id   = [] :: [] | binary(),
                         dev_key     = [] :: [] | binary(),
                         user_id     = [] :: [] | binary(),
                         token       = [] :: [] | binary(),
                         data        = [] :: [] | binary(),
                         type        = [] :: authType(),
                         attempts    = [] :: [] | integer(),
                         settings    = [] :: list(#'Feature'{}),
                         push        = [] :: [] | binary(),
                         os          = ios :: authOs(),
                         created     = [] :: [] | integer(),
                         last_online = [] :: [] | integer() }).

-record('AuthError',    {codes    = [] :: list(authStatus()),
                         data     = [] :: [] | #'Auth'{}}).

-record('Service',      {id        = [] :: binary(),
                        data       = [] :: binary(),
                        type       = [] :: serverType(),
                        setting    = [] :: list(#'Feature'{}),
                        expiration = [] :: integer(),
                        service_status     = [] :: serverStatus()}).

-record('Member',       {id        = [] :: [] | integer(),
                         container = chain :: container(),
                         feed_id   = [] :: #muc{} | #p2p{},
                         prev      = [] :: [] | integer(),
                         next      = [] :: [] | integer(),
                         feeds     = [] :: list(),
                         phone_id  = [] :: [] | binary(),
                         avatar    = [] :: [] | binary(),
                         names     = [] :: [] | binary(),
                         surnames  = [] :: [] | binary(),
                         alias     = [] :: [] | binary(),
                         reader    = 0  :: [] | integer(),
                         update    = 0  :: [] | integer(),
                         settings  = [] :: list(#'Feature'{}),
                         services  = [] :: list(#'Service'{}),
                         presence  = offline :: presence(),
                         member_status    = member :: memberStatus()}).

-record('Link',         {id        = [] :: [] | binary(),
                         name      = [] :: [] | binary(),
                         room_id   = [] :: [] | binary(),
                         created   = 0  :: [] | integer(),
                         links_status    = [] :: linkStatus()}).

-record('Room',         {id          = [] :: [] | binary(),
                         name        = [] :: [] | binary(),
                         links       = [] :: [] | list(#'Link'{}),
                         description = [] :: [] | binary(),
                         settings    = [] :: list(#'Feature'{}),
                         members     = [] :: list(#'Member'{}),
                         admins      = [] :: list(#'Member'{}),
                         data        = [] :: list(#'Desc'{}),
                         type        = [] :: roomType(),
                         tos         = [] :: [] | binary(),
                         tos_update  = 0  :: [] | integer(),
                         unread      = 0  :: [] | integer(),
                         mentions    = [] :: list(integer()),
                         readers     = [] :: list(integer()),
                         last_msg    = [] :: [] | #'Message'{},
                         update      = 0  :: [] | integer(),
                         created     = 0  :: [] | integer(),
                         status      = [] :: roomStatus()}).

-record('Contact',      {user_id = [] :: [] | binary(),
                         avatar   = [] :: list(#'Desc'{}),
                         names    = [] :: [] | binary(),
                         surnames = [] :: [] | binary(),
                         nick     = [] :: [] | binary(),
                         reader   = [] :: [] | list(integer()),
                         unread   = 0  :: [] | integer(),
                         last_msg = [] :: [] | #'Message'{},
                         update   = 0  :: [] | integer(),
                         created  = 0  :: [] | integer(),
                         settings = [] :: list(#'Feature'{}),
                         services = [] :: list(#'Service'{}),
             		         presence = offline :: presence(),
                         status   = [] :: contactStatus()}).

-record('Star',         {id        = [] :: [] | integer(),
                         client_id = [] :: [] | binary(),
                         roster_id = [] :: [] | integer(),
                         message   = [] :: #'Message'{},
                         tags      = [] :: list(#'Tag'{}),
                         status    = [] :: starType()}).

-record('ExtendedStar', {star      = [] :: #'Star'{},
                         from      = [] :: #'Contact'{} | #'Room'{}}).

-record('Roster',       {id       = [] :: [] | integer(),
                         names    = [] :: [] | binary(),
                         surnames = [] :: [] | binary(),
                         email    = [] :: [] | binary(),
                         nick     = [] :: [] | binary(),
                         userlist = [] :: list(#'Contact'{}),
                         roomlist = [] :: list(#'Room'{}),
                         favorite = [] :: list(#'ExtendedStar'{}),
                         tags     = [] :: list(#'Tag'{}),
                         phone    = [] :: [] | binary(),
                         avatar   = [] :: [] | binary(),
                         update   = 0  :: [] | integer(),
                         rosterStatus   = [] :: rosterStatus() }).

-record('Profile',      {phone    = [] :: [] | binary(),
                         services = [] :: list(#'Service'{}),
                         rosters  = [] :: list(#'Roster'{}),
                         settings = [] :: list(#'Feature'{}),
                         update   = 0  :: integer(),
                         balance  = 0  :: integer(),
                         presence = offline :: presence(),
                         profileStatus   = [] :: profileStatus() }).


-endif.
