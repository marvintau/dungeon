update char_chest set last_opened_chest = last_opened_chest % 4 + 1, last_opened_time=now()
where char_id = '85f6d769-713b-48ad-9163-7ba43b7459c7';

drop table opened_chest;

select
    char_id, chest_id, chest_name, max_item_types, min_item_types,
    round(random() * (max_item_types - min_item_types)) + min_item_types as item_types
into
    opened_chest
from
    char_chest
inner join chest_spec on char_chest.last_opened_chest = chest_spec.chest_id
where char_id = '85f6d769-713b-48ad-9163-7ba43b7459c7';

select * from opened_chest;

select item_id, item_name, items
FROM
(SELECT
    tem.item_id, item_name, items, generate_series(1, drop_rate/5) as nah
from
    (select
        char_id, opened_chest.chest_id, chest_name, item_id, drop_rate,
        round(random() * (max_items - min_items) + min_items) as items
    from
        opened_chest
    inner join item_from_chest on item_from_chest.chest_id = opened_chest.chest_id
    ) as tem
inner join
    item_description on tem.item_id = item_description.item_id) as populated
group by item_id, item_name, items
order by random()
