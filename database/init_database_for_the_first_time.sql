
------------------------------------------------------------
-- Adding new player data
------------------------------------------------------------

drop table if exists player_profile CASCADE;

create table player_profile (
    id uuid,
    profile jsonb,

    unique(id)
);

insert into player_profile(id, profile) values
( 'f2740862-674b-479e-b02c-500e8a0285a0',
 '{"player_name": "Albert Einstein", "player_level": 58,
  "player_head" : "normal_hunter",
  "default_card" : "946ae77c-183b-4538-b439-ac9036024676",
  "default_skills" : ["talisman_of_death", "rune_of_the_void", "talisman_of_spellshrouding", "holy_hand_grenade", "poison_gas"],
  "card_list" : [
        "946ae77c-183b-4538-b439-ac9036024676",
        "15d715a8-d585-48fc-a65a-286fc41c9a3f",
        "a0c1a883-2995-4526-856c-26870e5b3f74",
        "be2d65f0-3c93-457e-8180-de7c93a365a5",
        "a009e5e9-2057-4353-9871-309d68752c1b",
        "db1c75ca-aa32-4f2b-9bb1-355267d4a2ad",
        "849d31be-b9cd-494c-9ccd-7cc656153b57",
        "1b0cf5e0-2164-46fd-8424-2146fca99fb9"]}'),
( '8673cc53-e2a8-4375-b6a3-007e2ebe6d5f',
 '{"player_name": "Max Planc", "player_level": 58,
  "player_head" : "normal_rogue",
  "default_card" : "a0c1a883-2995-4526-856c-26870e5b3f74",
  "default_skills" : ["talisman_of_death", "rune_of_the_void", "talisman_of_spellshrouding", "holy_hand_grenade", "poison_gas"],
  "card_list" : [
        "946ae77c-183b-4538-b439-ac9036024676",
        "15d715a8-d585-48fc-a65a-286fc41c9a3f",
        "a0c1a883-2995-4526-856c-26870e5b3f74",
        "be2d65f0-3c93-457e-8180-de7c93a365a5",
        "a009e5e9-2057-4353-9871-309d68752c1b",
        "db1c75ca-aa32-4f2b-9bb1-355267d4a2ad",
        "849d31be-b9cd-494c-9ccd-7cc656153b57",
        "1b0cf5e0-2164-46fd-8424-2146fca99fb9"]}'),
( '68b19bbe-bc2a-400f-b4e7-6e632b3b908f',
 '{"player_name": "Erwin Schodinger", "player_level": 58,
  "player_head" : "normal_mage",
  "default_card" : "a009e5e9-2057-4353-9871-309d68752c1b",
  "default_skills" : ["talisman_of_death", "rune_of_the_void", "talisman_of_spellshrouding", "holy_hand_grenade", "poison_gas"],
  "card_list" : [
        "946ae77c-183b-4538-b439-ac9036024676",
        "15d715a8-d585-48fc-a65a-286fc41c9a3f",
        "a0c1a883-2995-4526-856c-26870e5b3f74",
        "be2d65f0-3c93-457e-8180-de7c93a365a5",
        "a009e5e9-2057-4353-9871-309d68752c1b",
        "db1c75ca-aa32-4f2b-9bb1-355267d4a2ad",
        "849d31be-b9cd-494c-9ccd-7cc656153b57",
        "1b0cf5e0-2164-46fd-8424-2146fca99fb9"]}'),
( 'db863aec-ee33-4161-884b-ef7109db4d25',
 '{"player_name": "Wolfgang Polly", "player_level": 58,
  "player_head" : "normal_warrior",
  "default_card" : "1b0cf5e0-2164-46fd-8424-2146fca99fb9",
  "default_skills" : ["talisman_of_death", "rune_of_the_void", "talisman_of_spellshrouding", "holy_hand_grenade", "poison_gas"],
  "card_list" : [
        "946ae77c-183b-4538-b439-ac9036024676",
        "15d715a8-d585-48fc-a65a-286fc41c9a3f",
        "a0c1a883-2995-4526-856c-26870e5b3f74",
        "be2d65f0-3c93-457e-8180-de7c93a365a5",
        "a009e5e9-2057-4353-9871-309d68752c1b",
        "db1c75ca-aa32-4f2b-9bb1-355267d4a2ad",
        "849d31be-b9cd-494c-9ccd-7cc656153b57",
        "1b0cf5e0-2164-46fd-8424-2146fca99fb9"]}');

select * from player_profile;

drop table if exists character_card_profile CASCADE;

CREATE TABLE character_card_profile (
    id uuid,	  
    profile jsonb,
    
    unique(id)
);

insert into character_card_profile (id, profile) values
( '946ae77c-183b-4538-b439-ac9036024676',
'{"hp": 1300, "card_name":"普通刺客", "image_name": "normal_rogue", "class": "rogue", "range_type" : "near",
 "agi": 75, "hit": 35, "armor": 4500, "block": 0, "dodge": 30, "resist": 35, "critical": 30,
 "prim_type": "physical", "prim_max": 205, "prim_min": 190, "secd_type": "physical", "secd_max": 190, "secd_min": 175,
 "cast_list": ["talisman_of_death", "rune_of_the_void", "talisman_of_spellshrouding", "holy_hand_grenade", "poison_gas", "healing_potion", "pierce_armor", "flurry", "spellbreak", "perfect_strike"],
 "talented_skill": "blade_dance"}'),

( '15d715a8-d585-48fc-a65a-286fc41c9a3f',
'{"hp": 1300, "card_name":"霸道刺客", "image_name": "awaken_rogue", "class": "rogue", "range_type" : "near",
 "agi": 75, "hit": 35, "armor": 4500, "block": 0, "dodge": 30, "resist": 35, "critical": 30,
 "prim_type": "physical", "prim_max": 205, "prim_min": 190, "secd_type": "physical", "secd_max": 190, "secd_min": 175,
 "cast_list": ["talisman_of_death", "rune_of_the_void", "talisman_of_spellshrouding", "holy_hand_grenade", "poison_gas", "healing_potion", "pierce_armor", "flurry", "spellbreak", "perfect_strike"],
 "talented_skill": "blade_dance"}'),

('a0c1a883-2995-4526-856c-26870e5b3f74',
'{"hp": 1400, "card_name":"普通猎人", "image_name": "normal_hunter", "class": "hunter", "range_type" : "far",
 "agi": 40, "hit": 35, "armor": 4500, "block": 0, "dodge": 30, "resist": 35, "critical": 30,
 "prim_type": "physical", "prim_max": 370, "prim_min": 335, "secd_type": "bare", "secd_max": 130, "secd_min": 60,
 "cast_list": ["talisman_of_death", "rune_of_the_void", "talisman_of_spellshrouding", "holy_hand_grenade", "poison_gas", "tornado", "mend", "outbreak", "roots", "tree_hide"],
 "talented_skill": "assault"}'),

('be2d65f0-3c93-457e-8180-de7c93a365a5',
'{"hp": 1400, "card_name":"痴呆猎人", "image_name": "awaken_hunter", "class": "hunter", "range_type" : "far",
 "agi": 40, "hit": 35, "armor": 4500, "block": 0, "dodge": 30, "resist": 35, "critical": 30,
 "prim_type": "physical", "prim_max": 370, "prim_min": 335, "secd_type": "bare", "secd_max": 130, "secd_min": 60,
 "cast_list": ["talisman_of_death", "rune_of_the_void", "talisman_of_spellshrouding", "holy_hand_grenade", "poison_gas", "tornado", "mend", "outbreak", "roots", "tree_hide"],
 "talented_skill": "assault"}'),

('a009e5e9-2057-4353-9871-309d68752c1b',
 '{"hp": 1600, "card_name":"普通法师", "image_name": "normal_mage", "class": "mage", "range_type" : "far",
 "agi": 35, "hit": 20, "armor": 2700, "block": 0, "dodge": 20, "resist": 15, "critical": 35,
 "prim_type": "magic", "prim_max": 330, "prim_min": 275, "secd_type": "bare", "secd_max": 100, "secd_min": 50,
 "cast_list": ["talisman_of_death", "rune_of_the_void", "talisman_of_spellshrouding", "holy_hand_grenade", "poison_gas", "vampiric_bolt", "arcane_surge", "lower_resist", "pyromania", "mind_blast"],
 "talented_skill": "freeze"}'),

('db1c75ca-aa32-4f2b-9bb1-355267d4a2ad',
 '{"hp": 1600, "card_name":"暴躁法师", "image_name": "awaken_mage", "class": "mage", "range_type" : "far",
 "agi": 35, "hit": 20, "armor": 2700, "block": 0, "dodge": 20, "resist": 15, "critical": 35,
 "prim_type": "magic", "prim_max": 330, "prim_min": 275, "secd_type": "bare", "secd_max": 100, "secd_min": 50,
 "cast_list": ["talisman_of_death", "rune_of_the_void", "talisman_of_spellshrouding", "holy_hand_grenade", "poison_gas", "vampiric_bolt", "arcane_surge", "lower_resist", "pyromania", "mind_blast"],
 "talented_skill": "freeze"}'),

('849d31be-b9cd-494c-9ccd-7cc656153b57',
 '{"hp": 1800, "card_name":"普通战士", "image_name": "normal_warrior", "class": "warrior", "range_type" : "near",
 "agi": 50, "hit": 35, "armor": 4500, "block": 0, "dodge": 30, "resist": 35, "critical": 30,
 "prim_type": "physical", "prim_max": 205, "prim_min": 190, "secd_type": "shield", "secd_max": 190, "secd_min": 175,
 "cast_list": ["talisman_of_death", "rune_of_the_void", "talisman_of_spellshrouding", "holy_hand_grenade", "poison_gas", "shield_wall", "sure_hit", "double_swing", "chain_lock", "first_aid"], 
 "talented_skill": "brave_shield_counterback"}'),

('1b0cf5e0-2164-46fd-8424-2146fca99fb9',
 '{"hp": 1800, "card_name":"癫狂战士", "image_name": "awaken_warrior", "class": "warrior", "range_type" : "near",
 "agi": 50, "hit": 35, "armor": 4500, "block": 0, "dodge": 30, "resist": 35, "critical": 30,
 "prim_type": "physical", "prim_max": 205, "prim_min": 190, "secd_type": "shield", "secd_max": 190, "secd_min": 175,
 "cast_list": ["talisman_of_death", "rune_of_the_void", "talisman_of_spellshrouding", "holy_hand_grenade", "poison_gas", "shield_wall", "sure_hit", "double_swing", "chain_lock", "first_aid"],
 "talented_skill": "brave_shield_counterback"}');

select id, profile as name from character_card_profile;


------------------------------------------------------------
-- Adding chest specification
------------------------------------------------------------

drop table if exists chest_spec cascade;

create table chest_spec (
    chest_id int,
    chest_name CHARACTER VARYING(40),
    min_item_types int,
    max_item_types int,
    open_interval int,

    UNIQUE(chest_id)
);

copy chest_spec(chest_id, chest_name, min_item_types, max_item_types, open_interval)
from '/home/marvin/dungeon/database/preset_data/chest_specs.csv' delimiter ',' csv header;

select * from chest_spec;

drop table if exists item_from_chest;

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
from '/home/marvin/dungeon/database/preset_data/chest-golden.csv' delimiter ',' csv header;
copy item_from_chest(chest_id, item_id, min_items, max_items, drop_rate)
from '/home/marvin/dungeon/database/preset_data/chest-bronze.csv' delimiter ',' csv header;
copy item_from_chest(chest_id, item_id, min_items, max_items, drop_rate)
from '/home/marvin/dungeon/database/preset_data/chest-wood.csv' delimiter ',' csv header;
copy item_from_chest(chest_id, item_id, min_items, max_items, drop_rate)
from '/home/marvin/dungeon/database/preset_data/chest-silver.csv' delimiter ',' csv header;
copy item_from_chest(chest_id, item_id, min_items, max_items, drop_rate)
from '/home/marvin/dungeon/database/preset_data/chest-iron.csv' delimiter ',' csv header;

select * from item_from_chest;

drop table if exists item_description;

create table item_description(
    item_id int,
    item_name character varying(40),
    item_desc character varying(255),
    image_name character varying(50)
);

copy item_description(item_id, item_name, item_desc, image_name)
from '/home/marvin/dungeon/database/preset_data/item-names.csv' delimiter ',' csv header;

select * from item_description;



------------------------------------------------------------
-- Adding chest opening profile
------------------------------------------------------------
drop table char_chest cascade;

create table char_chest(
    char_id uuid,
    last_opened_chest int,
    last_opened_time TIMESTAMP,
    is_today_done BOOLEAN,

    foreign key(char_id) references player_profile(id),
    foreign key(last_opened_chest) references chest_spec(chest_id)
);

insert into char_chest(char_id, last_opened_chest, last_opened_time, is_today_done) values
('f2740862-674b-479e-b02c-500e8a0285a0', '0', now(), 'no'),
('8673cc53-e2a8-4375-b6a3-007e2ebe6d5f', '0', now(), 'no'),
('68b19bbe-bc2a-400f-b4e7-6e632b3b908f', '0', now(), 'no'),
('db863aec-ee33-4161-884b-ef7109db4d25', '0', now(), 'no');

select * from char_chest ;
