
drop table if exists player_profile CASCADE;

create table player_profile (
    id uuid,
    profile jsonb,

    unique(id)
);

insert into player_profile(id, profile) values
( '68c28fc2-d512-4779-be00-0cc0fec0de9f',
 '{"player_name": "李逍遥", "player_level": 58,
  "default_card" : "946ae77c-183b-4538-b439-ac9036024676",
  "card_list" : [
        "946ae77c-183b-4538-b439-ac9036024676",
        "15d715a8-d585-48fc-a65a-286fc41c9a3f",
        "a0c1a883-2995-4526-856c-26870e5b3f74",
        "be2d65f0-3c93-457e-8180-de7c93a365a5",
        "a009e5e9-2057-4353-9871-309d68752c1b",
        "db1c75ca-aa32-4f2b-9bb1-355267d4a2ad",
        "849d31be-b9cd-494c-9ccd-7cc656153b57",
        "1b0cf5e0-2164-46fd-8424-2146fca99fb9"]}'),
( 'cbca87fa-0a52-4d63-a248-17c6faf6f321',
 '{"player_name": "姜云凡", "player_level": 63,
  "default_card" : "be2d65f0-3c93-457e-8180-de7c93a365a5",
  "card_list" : [
        "946ae77c-183b-4538-b439-ac9036024676",
        "15d715a8-d585-48fc-a65a-286fc41c9a3f",
        "be2d65f0-3c93-457e-8180-de7c93a365a5",
        "a009e5e9-2057-4353-9871-309d68752c1b",
        "1b0cf5e0-2164-46fd-8424-2146fca99fb9"]}');

drop table if exists character_card_profile CASCADE;

CREATE TABLE character_card_profile (
    id uuid,	  
    profile jsonb,
    
    unique(id)
);

insert into character_card_profile (id, profile) values
( '946ae77c-183b-4538-b439-ac9036024676',
'{"hp": 2700, "card_name":"普通刺客", "image_name": "normal_rogue", "class": "rogue", "range_type" : "near",
 "agi": 75, "hit": 35, "armor": 4500, "block": 0, "dodge": 30, "resist": 35, "critical": 30,
 "prim_type": "physical", "prim_max": 205, "prim_min": 190, "secd_type": "physical", "secd_max": 190, "secd_min": 175,
 "cast_list": ["poison_gas", "talisman_of_spellshrouding", "talisman_of_death", "holy_hand_grenade", "none"],
 "talented_skill": "blade_dance"}'),

( '15d715a8-d585-48fc-a65a-286fc41c9a3f',
'{"hp": 2700, "card_name":"霸道刺客", "image_name": "awaken_rogue", "class": "rogue", "range_type" : "near",
 "agi": 75, "hit": 35, "armor": 4500, "block": 0, "dodge": 30, "resist": 35, "critical": 30,
 "prim_type": "physical", "prim_max": 205, "prim_min": 190, "secd_type": "physical", "secd_max": 190, "secd_min": 175,
 "cast_list": ["poison_gas", "talisman_of_spellshrouding", "talisman_of_death", "holy_hand_grenade", "none"],
 "talented_skill": "blade_dance"}'),

('a0c1a883-2995-4526-856c-26870e5b3f74',
'{"hp": 3400, "card_name":"普通猎人", "image_name": "normal_hunter", "class": "hunter", "range_type" : "far",
 "agi": 40, "hit": 35, "armor": 4500, "block": 0, "dodge": 30, "resist": 35, "critical": 30,
 "prim_type": "physical", "prim_max": 370, "prim_min": 335, "secd_type": "bare", "secd_max": 100, "secd_min": 50,
 "cast_list": ["poison_gas", "talisman_of_spellshrouding", "talisman_of_death", "holy_hand_grenade", "none"],
 "talented_skill": "blade_dance"}'),

('be2d65f0-3c93-457e-8180-de7c93a365a5',
'{"hp": 3400, "card_name":"痴呆猎人", "image_name": "awaken_hunter", "class": "hunter", "range_type" : "far",
 "agi": 40, "hit": 35, "armor": 4500, "block": 0, "dodge": 30, "resist": 35, "critical": 30,
 "prim_type": "physical", "prim_max": 370, "prim_min": 335, "secd_type": "bare", "secd_max": 100, "secd_min": 50,
 "cast_list": ["poison_gas", "talisman_of_spellshrouding", "talisman_of_death", "holy_hand_grenade", "none"],
 "talented_skill": "blade_dance"}'),

('a009e5e9-2057-4353-9871-309d68752c1b',
 '{"hp": 2300, "card_name":"普通法师", "image_name": "normal_mage", "class": "mage", "range_type" : "far",
 "agi": 35, "hit": 20, "armor": 2700, "block": 0, "dodge": 20, "resist": 15, "critical": 35,
 "prim_type": "magic", "prim_max": 280, "prim_min": 255, "secd_type": "bare", "secd_max": 100, "secd_min": 50,
 "cast_list": ["poison_gas", "talisman_of_spellshrouding", "talisman_of_death", "holy_hand_grenade", "none"],
 "talented_skill": "blade_dance"}'),

('db1c75ca-aa32-4f2b-9bb1-355267d4a2ad',
 '{"hp": 2300, "card_name":"暴躁法师", "image_name": "awaken_mage", "class": "mage", "range_type" : "far",
 "agi": 35, "hit": 20, "armor": 2700, "block": 0, "dodge": 20, "resist": 15, "critical": 35,
 "prim_type": "magic", "prim_max": 280, "prim_min": 255, "secd_type": "bare", "secd_max": 100, "secd_min": 50,
 "cast_list": ["poison_gas", "talisman_of_spellshrouding", "talisman_of_death", "holy_hand_grenade", "none"],
 "talented_skill": "blade_dance"}'),

('849d31be-b9cd-494c-9ccd-7cc656153b57',
 '{"hp": 3400, "card_name":"普通战士", "image_name": "normal_warrior", "class": "warrior", "range_type" : "near",
 "agi": 50, "hit": 35, "armor": 4500, "block": 0, "dodge": 30, "resist": 35, "critical": 30,
 "prim_type": "physical", "prim_max": 205, "prim_min": 190, "secd_type": "physical", "secd_max": 190, "secd_min": 175,
 "cast_list": ["poison_gas", "talisman_of_spellshrouding", "talisman_of_death", "holy_hand_grenade", "none"],
 "talented_skill": "blade_dance"}'),

('1b0cf5e0-2164-46fd-8424-2146fca99fb9',
 '{"hp": 3400, "card_name":"癫狂战士", "image_name": "awaken_warrior", "class": "warrior", "range_type" : "near",
 "agi": 50, "hit": 35, "armor": 4500, "block": 0, "dodge": 30, "resist": 35, "critical": 30,
 "prim_type": "physical", "prim_max": 205, "prim_min": 190, "secd_type": "physical", "secd_max": 190, "secd_min": 175,
 "cast_list": ["poison_gas", "talisman_of_spellshrouding", "talisman_of_death", "holy_hand_grenade", "none"],
 "talented_skill": "blade_dance"}');


select id, profile as name from character_card_profile;

drop table char_chest cascade;

create table char_chest(
    char_id uuid,
    last_opened_chest int,
    last_opened_time TIMESTAMP,

    foreign key(char_id) references player_profile(id),
    foreign key(last_opened_chest) references chest_spec(chest_id)
);

insert into char_chest(char_id, last_opened_chest, last_opened_time) values
('68c28fc2-d512-4779-be00-0cc0fec0de9f', '1', now()),
('cbca87fa-0a52-4d63-a248-17c6faf6f321', '1', now());

-- select * from char_chest ;

-- drop table if exists chest_spec cascade;

-- create table chest_spec (
--     chest_id int,
--     chest_name CHARACTER VARYING(40),
--     min_item_types int,
--     max_item_types int,
--     open_interval int,

--     UNIQUE(chest_id)
-- );

-- copy chest_spec(chest_id, chest_name, min_item_types, max_item_types, open_interval)
-- from '/Users/marvin/Github/dungeon/database/preset_data/chest_specs.csv' delimiter ',' csv header;

-- select * from chest_spec;

-- drop table if exists item_from_chest;

-- create table item_from_chest (
--     item_id int,
--     chest_id int,
--     min_items int,
--     max_items int,
--     drop_rate int,
    
--     -- foreign key(item_id) references,
--     foreign key(chest_id) references chest_spec(chest_id)
-- );

-- copy item_from_chest(chest_id, item_id, min_items, max_items, drop_rate)
-- from '/Users/marvin/Github/dungeon/database/preset_data/chest-golden.csv' delimiter ',' csv header;
-- copy item_from_chest(chest_id, item_id, min_items, max_items, drop_rate)
-- from '/Users/marvin/Github/dungeon/database/preset_data/chest-bronze.csv' delimiter ',' csv header;
-- copy item_from_chest(chest_id, item_id, min_items, max_items, drop_rate)
-- from '/Users/marvin/Github/dungeon/database/preset_data/chest-wood.csv' delimiter ',' csv header;
-- copy item_from_chest(chest_id, item_id, min_items, max_items, drop_rate)
-- from '/Users/marvin/Github/dungeon/database/preset_data/chest-silver.csv' delimiter ',' csv header;
-- copy item_from_chest(chest_id, item_id, min_items, max_items, drop_rate)
-- from '/Users/marvin/Github/dungeon/database/preset_data/chest-iron.csv' delimiter ',' csv header;

-- select * from item_from_chest;

-- drop table if exists item_description;

-- create table item_description(
--     item_id int,
--     item_name character varying(40),
--     item_desc character varying(255),
--     image_name character varying(50)
-- );

-- copy item_description(item_id, item_name, item_desc, image_name)
-- from '/Users/marvin/Github/dungeon/database/preset_data/item-names.csv' delimiter ',' csv header;

-- select * from item_description;