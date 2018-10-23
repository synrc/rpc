-ifndef(ROSTER_HRL).
-define(ROSTER_HRL, true).

-type authType()  :: google_api | facebook_api | mobile | email |
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
                         type        = email :: authStatus(),
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

-endif.
