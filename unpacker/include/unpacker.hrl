-record(tv, {
          show :: string(),
          season :: integer(),
          episode :: integer(),
          year :: integer(),
          torrent :: string()
         }).

-record(movie, {
          title :: string(),
          year :: integer(),
          torrent :: string()
         }).
