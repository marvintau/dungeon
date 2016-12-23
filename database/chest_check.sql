select
     interval '1h' * open_interval - age(now(), last_opened_time)
from
    char_chest
inner join chest_spec on char_chest.last_opened_chest = chest_spec.chest_id
where
    char_id = '1';