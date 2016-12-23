drop table chest_spec CASCADE;

create table chest_spec (
    chest_id int,
    chest_name CHARACTER VARYING(40),
    min_item_types int,
    max_item_types int,
    open_interval int,

    UNIQUE(chest_id)
);

copy chest_spec(chest_id, chest_name, min_item_types, max_item_types, open_interval)
from '/Users/yuetao/projects/dungeon/database/preset_data/chest_specs.csv' delimiter ',' csv header;

select * from chest_spec;

drop table item_from_chest;

create table item_from_chest (
    item_id int,
    chest_id int,
    min_items int,
    max_items int,
    drop_rate int,
    
    -- foreign key(item_id) references,
    foreign key(chest_id) references chest_spec(chest_id)
);

copy item_from_chest(chest_id, item_id, min_items, max_items, drop_rate)
from '/Users/yuetao/projects/dungeon/database/preset_data/chest-golden.csv' delimiter ',' csv header;
copy item_from_chest(chest_id, item_id, min_items, max_items, drop_rate)
from '/Users/yuetao/projects/dungeon/database/preset_data/chest-bronze.csv' delimiter ',' csv header;
copy item_from_chest(chest_id, item_id, min_items, max_items, drop_rate)
from '/Users/yuetao/projects/dungeon/database/preset_data/chest-wood.csv' delimiter ',' csv header;
copy item_from_chest(chest_id, item_id, min_items, max_items, drop_rate)
from '/Users/yuetao/projects/dungeon/database/preset_data/chest-silver.csv' delimiter ',' csv header;
copy item_from_chest(chest_id, item_id, min_items, max_items, drop_rate)
from '/Users/yuetao/projects/dungeon/database/preset_data/chest-iron.csv' delimiter ',' csv header;

select * from item_from_chest;

drop table item_description;

create table item_description(
    item_id int,
    item_name character varying(40),
    item_desc character varying(255),
    image_name character varying(50)
);

copy item_description(item_id, item_name, item_desc, image_name)
from '/Users/yuetao/projects/dungeon/database/preset_data/item-names.csv' delimiter ',' csv header;

select * from item_description;