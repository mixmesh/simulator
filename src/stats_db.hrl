-ifndef(STATS_DB_HRL).
-define(STATS_DB_HRL, true).

-record(stats_db_analysis,
        {start_time,
         current_time,
         created_messages,
         delivered_messages,
         delivery_delays,
         relayed_messages}).


-endif.
