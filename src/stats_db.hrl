-ifndef(STATS_DB_HRL).
-define(STATS_DB_HRL, true).

-record(stats_db_analysis,
        {start_time,
         current_time,
         created_messages = 0,
         delivered_messages = 0,
         delivery_delays = [],
         relayed_messages = 0,
         overwritten_messages = 0}).

-endif.
