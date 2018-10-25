-ifndef(FEATURE_HRL).
-define(FEATURE_HRL, true).

-type presence()      :: online | offline.
-type serverType()    :: email | wallet | google_type | fb | phone.
-type serverStatus()  :: servie_verified | service_not_verified.

-record('Feature',      {id    = [] :: [] | binary(),
                         key   = [] :: [] | binary(),
                         value = [] :: [] | binary(),
                         group = [] :: [] | binary()}).

-record('Service',      {id        = [] :: binary(),
                        data       = [] :: binary(),
                        type       = [] :: serverType(),
                        setting    = [] :: list(#'Feature'{}),
                        expiration = [] :: integer(),
                        service_status     = [] :: serverStatus()}).

-endif.
