-ifndef(ROSTER_HRL).
-define(ROSTER_HRL, true).

-type presence()  :: online | offline.

-record('Feature',      {id    = [] :: [] | binary(),
                         key   = [] :: [] | binary(),
                         value = [] :: [] | binary(),
                         group = [] :: [] | binary()}).

%%================================================AUTH RECORD ==========================================================
-type authOs() :: ios | android | web.

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



-type authType()  :: google_auth | facebook_auth | mobile_auth | email_auth |
voice | resend | verify | push | logout | get | delete | clear.

-type authStatus() :: invalid_version | mismatch_user_data | number_not_allowed |
session_not_found | attempts_expired | invalid_sms_code |
invalid_jwt_code | permission_denied | invalid_data.

-record('Service',      {id        = [] :: binary(),
                        data       = [] :: binary(),
                        type       = [] :: serverType(),
                        setting    = [] :: list(#'Feature'{}),
                        expiration = [] :: integer(),
                        status     = [] :: serverStatus()}).

-type serverType() ::  email | wallet | google_type | fb | phone.

-type serverStatus() :: servie_verified | service_not_verified.

%%================================================CONTACT RECORD =======================================================
-record('Contact',      {user_id = [] :: [] | binary(),
%%                         avatar   = [] : list(#'Desc'{}),
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
                         status   = [] :: contactStatus()}).

-type contactStatus() :: conact_request | authorization | contact_ignore | conatct_internal | friend | contact_last_msg
| contact_ban | conact_banned | contact_deleted.

%%================================================ROSTER RECORD ========================================================

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
                         rosterStatus   = [] :: rosterStatus() }).

-type rosterStatus() :: get_roster | create_roster | del_roster | remove_roster| nick| add_roster | update_roster |
list_loster | patch_roster | roster_last_msg.

%%================================================PROFILE RECORD ========================================================

-record('Profile',      {phone    = [] :: [] | binary(),
                         services = [] :: list(#'Service'{}),
                         rosters  = [] :: list(#'Roster'{}),
                         settings = [] :: list(#'Feature'{}),
                         update   = 0  :: integer(),
                         balance  = 0  :: integer(),
                         presence = offline :: presence(),
                         profileStatus   = [] :: profileStatus() }).

-type profileStatus() :: remove_profile | get_profile | patch_profile.

-endif.
