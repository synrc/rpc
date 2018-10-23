-ifndef(ROSTER_HRL).
-define(ROSTER_HRL, true).

-type presence()  :: online | offline.

-type authType()  :: google_auth | facebook_auth | mobile_auth | email_auth |
                     voice | resend | verify | push | logout | get | delete | clear.

-type authStatus() :: invalid_version | mismatch_user_data | number_not_allowed |
                     session_not_found | attempts_expired | invalid_sms_code |
                     invalid_jwt_code | permission_denied | invalid_data.

-record('Feature',      {id    = [] :: [] | binary(),
                         key   = [] :: [] | binary(),
                         value = [] :: [] | binary(),
                         group = [] :: [] | binary()}).

-record('Auth',         {client_id   = [] :: [] | binary(),
                         dev_key     = [] :: [] | binary(),
                         user_id     = [] :: [] | binary(),
                         phone       = [] :: [] | binary(),
                         token       = [] :: [] | binary(),
                         type        = email :: authType(),
                         sms_code    = [] :: [] | binary(),
                         attempts    = [] :: [] | integer(),
                         services    = [] :: list(atom()),
                         settings    = [] :: list(#'Feature'{}),
                         push        = [] :: [] | binary(),
                         os          = [] :: [] | ios | android | web,
                         created     = [] :: [] | integer(),
                         last_online = [] :: [] | integer() }).

-record('AuthError',   {codes    = [] :: list(authStatus()),
                        data     = [] :: [] | #'Auth'{}}).

-record('Service',      {id        = [] :: [] | binary(),
                        type       = [] :: [] | email | vox | aws | wallet,
                        data       = [] :: term(),
                        login      = [] :: [] | binary(),
                        password   = [] :: [] | binary(),
                        expiration = [] :: [] | integer(),
                        serviceStatus     = [] :: [] | verified | added}).

-record('Contact',      {phone_id = [] :: [] | binary(),
                         avatar   = [] :: [] | binary(),
                         names    = [] :: [] | binary(),
                         surnames = [] :: [] | binary(),
                         nick     = [] :: [] | binary(),
                         reader   = [] :: [] | list(integer()),
                         unread   = 0  :: [] | integer(),
%                         last_msg = [] :: [] | #'Message'{},
                         update   = 0  :: [] | integer(),
                         created  = 0  :: [] | integer(),
                         settings = [] :: list(#'Feature'{}),
                         services = [] :: list(#'Service'{}),
             		presence = offline :: presence(),
                         status   = [] :: [] | request | authorization | ignore | internal
                                             | friend | last_msg | ban | banned | deleted }).

-record('Roster',       {id       = [] :: [] | integer(),
                         names    = [] :: [] | binary(),
                         surnames = [] :: [] | binary(),
                         email    = [] :: [] | binary(),
                         nick     = [] :: [] | binary(),
                         userlist = [] :: list(#'Contact'{}),
%                         roomlist = [] :: list(#'Room'{}),
%                         favorite = [] :: list(#'ExtendedStar'{}),
%                         tags     = [] :: list(#'Tag'{}),
                         phone    = [] :: [] | binary(),
                         avatar   = [] :: [] | binary(),
                         update   = 0  :: [] | integer(),
                         rosterStatus   = [] :: [] | get_roster | create_roster | del_roster | remove_roster
                                                   | nick
                                             | add_roster | update_roster | list_loster | patch_roster | roster_last_msg }).

-record('Profile',      {phone    = [] :: [] | binary(),
                         services = [] :: list(#'Service'{}),
                         rosters  = [] :: list(#'Roster'{}),
                         settings = [] :: list(#'Feature'{}),
                         update   = 0  :: integer(),
                         balance  = 0  :: integer(),
                         presence = offline :: presence(),
                         profileStatus   = [] :: [] | remove_profile | get_profile | patch_profile }).

-endif.
